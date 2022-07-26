(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Make = struct
    type buffer = Cstruct.t
    type id = int
    type macaddr = Macaddr.t

    type c = {
        mutable callback_counter : int;
        cond : unit Eio.Condition.t;
        mutex : Eio.Mutex.t;
    }

    type t = {
        mutable last_id : int;
        mutable call_counter : int;
        yield : (unit -> unit);
        listener_callback : ((buffer -> unit) -> c -> buffer -> unit);
        listener_callbacks_in_progress : (int, c) Hashtbl.t;
        listeners : (int, buffer -> unit) Hashtbl.t;
        macs : (int, macaddr) Hashtbl.t;
        disconnect : unit -> unit;
    }

    let make_mac id =
        let i = Int32.of_int id in
        let byte v shr = Int32.to_int (Int32.logand (Int32.shift_right_logical v shr) 0xFFl) in
        let base_mac = [| 0 ; 0x50 ; (byte i 24) ; (byte i 16) ; (byte i 8) ; (byte i 0) |] in (* TODO Use different prefix? *)
        Macaddr.make_local (Array.get base_mac)

    let dec_callback_counter c =
        Eio.Mutex.with_lock c.mutex (
            fun () -> (c.callback_counter <- c.callback_counter - 1)
        );
        Eio.Condition.broadcast c.cond ()

    let inc_callback_counter c =
        Eio.Mutex.with_lock c.mutex (
            fun () -> (c.callback_counter <- c.callback_counter + 1)
        );
        Eio.Condition.broadcast c.cond ()


    let rec async_listener_callback sw stream =
        let (f, c, buffer) = Eio.Stream.take stream in
        inc_callback_counter c;
        Eio.Fiber.fork ~sw (fun () ->
            Eio.Private.Ctf.label "vnetif.async_listener_callback";
            f buffer; 
            dec_callback_counter c);
        async_listener_callback sw stream
    
    let create ?(yield=(fun () -> Eio.Fiber.yield ())) ?(use_async_readers) () =
        let last_id = 0 in
        let call_counter = 0 in
        let listeners = Hashtbl.create 7 in
        let macs = Hashtbl.create 7 in
        let listener_callbacks_in_progress = Hashtbl.create 7 in
        match use_async_readers with
        | Some sw ->
            let packet_stream = Eio.Stream.create max_int in
            let disconnect_promise, disconnect_resolve = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () -> 
              Eio.Fiber.first ~label:"vnetif.async_listener_callback.loop"
                (fun () -> async_listener_callback sw packet_stream)
                (fun () -> Eio.Promise.await disconnect_promise));
            let listener_callback f c buffer = Eio.Stream.add packet_stream (f, c, buffer)
            in
            let disconnect () = Eio.Promise.resolve disconnect_resolve () in
            {last_id;call_counter;listeners;macs;listener_callbacks_in_progress;yield;listener_callback;disconnect}
        | None ->
            let listener_callback f c buffer =
                inc_callback_counter c;
                f buffer;
                dec_callback_counter c
            in
            {last_id;call_counter;listeners;macs;listener_callbacks_in_progress;yield;listener_callback;disconnect=fun () -> ()}

    let disconnect t = t.disconnect ()
    
    let register t =
        t.last_id <- t.last_id + 1;
        Hashtbl.add t.macs t.last_id (make_mac t.last_id);
        Hashtbl.add t.listener_callbacks_in_progress t.last_id {
            callback_counter = 0;
            cond = Eio.Condition.create ~label:("vnetif.basic_backend.condition_"^string_of_int t.last_id) ();
            mutex = Eio.Mutex.create ~label:("vnetif.basic_backend.mutex_"^string_of_int t.last_id) () };
        t.last_id

    let unregister t id =
        Hashtbl.remove t.macs id;
        Hashtbl.remove t.listeners id;
        Hashtbl.remove t.listener_callbacks_in_progress id

    let wait_for_callbacks c =
        Eio.Mutex.with_lock c.mutex (fun () ->
            let rec loop = function
                | 0 -> ()
                | _ -> (Eio.Condition.await ~mutex:c.mutex c.cond |> ignore;
                       (loop c.callback_counter))
            in
            loop c.callback_counter
        )

    let unregister_and_flush t id =
        let c = Hashtbl.find t.listener_callbacks_in_progress id in
        unregister t id;
        wait_for_callbacks c

    let mac t id =
        Hashtbl.find t.macs id

    let set_listen_fn t id fn =
        Hashtbl.replace t.listeners id fn

    let buffers_copy src =
        let len = Cstruct.lenv src in
        let dst = Cstruct.create len in
        List.fold_left (fun index item -> 
            let len = Cstruct.length item in
            Cstruct.blit item 0 dst index len;
            index + len
        ) 0 src |> ignore;
        dst

    let writev t id iovec =
        let keys = Hashtbl.fold (fun k _v lst -> k::lst) t.listeners [] in
        let send t bufs src dst =
          if src != dst then
            begin
              t.call_counter <- t.call_counter + 1;
              let fn = (Hashtbl.find t.listeners dst) in
              let c = (Hashtbl.find t.listener_callbacks_in_progress dst) in
              t.listener_callback fn c (buffers_copy bufs)
            end 
          else
            ()
        in
        List.iter (send t iovec id) keys;
        t.yield ()

end
