(*
   Copyright (c) 2015-2020 Magnus Skjegstad <magnus@skjegstad.com>
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

open Mirage_net

let src = Logs.Src.create "vnetif" ~doc:"in-memory network interface"
module Log = (val Logs.src_log src : Logs.LOG)

module type BACKEND = sig
    type buffer = Cstruct.t
    type id = int
    type macaddr = Macaddr.t
    type t

    val register : t -> id
    val unregister : t -> id -> unit
    val mac : t -> id -> macaddr
    val writev : t -> id -> buffer list -> unit
    val set_listen_fn : t -> id -> (buffer -> unit) -> unit
    val unregister_and_flush : t -> id -> unit
end

module Make (B : BACKEND) = struct
  type t = {
    id : B.id;
    backend : B.t;
    mutable wake_on_disconnect : unit Eio.Promise.u option; (* woken up when disconnect is called, used by listen *)
    unlock_on_listen: Eio.Mutex.t option; (* unlocked when listen is called, used by tests *)
    size_limit : int option;
    stats : stats;
    monitor_fn : (B.buffer -> unit) option;
    flush_on_disconnect : bool;
  }

  let connect ?size_limit ?flush_on_disconnect:(flush_on_disconnect=false) ?monitor_fn ?unlock_on_listen backend =
    let id = B.register backend in
    let stats = { rx_bytes = 0L ; rx_pkts = 0l; tx_bytes = 0L; tx_pkts = 0l } in
    { id; size_limit; backend; stats; wake_on_disconnect=None; unlock_on_listen; monitor_fn; flush_on_disconnect }

  let mtu t = match t.size_limit with None -> 1500 | Some x -> x

  let disconnect t =
    (match t.flush_on_disconnect with
    | true -> B.unregister_and_flush t.backend t.id
    | false -> B.unregister t.backend t.id);
    (* If a listen call is blocking wake it up so it can exit *)
    match t.wake_on_disconnect with
    | None -> ()
    | Some e -> (Eio.Promise.resolve e ())

  let writev t iovec =
    let size = Cstruct.lenv iovec in
    let size =
      match t.size_limit with
      | Some l -> min size (l + 14)
      | None -> size
    in
    t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int size);
    t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
    (* Write copy of buffer to monitor function (if enabled) *)
    (match t.monitor_fn with
    | Some fn -> fn (Cstruct.concat iovec)
    | None -> ());
    B.writev t.backend t.id iovec

  
  let listen t ~header_size:_ fn =
    (* Add counters to the listener function *)
    let listener t fn buf =
      t.stats.rx_bytes <- Int64.add (Int64.of_int (Cstruct.length buf)) (t.stats.rx_bytes);
      t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
      fn buf
    in

    (* Wrap the backend listen function in a monitor_fn call when enabled *)
    (match t.monitor_fn with
    | None -> B.set_listen_fn t.backend t.id (listener t fn)
    | Some m_fn -> B.set_listen_fn t.backend t.id (fun buf -> m_fn buf; (listener t fn) buf));

    (* Unlock listener lock to allow tests to proceed *)
    (match t.unlock_on_listen with
    | None -> ()
    | Some l -> Eio.Mutex.unlock l);

    (* Block until woken up by disconnect *)
    let task, waker = Eio.Promise.create ~label:"Netif.listen" () in
    t.wake_on_disconnect <- (Some waker);
    Eio.Promise.await task

  let mac t =
    B.mac t.backend t.id

  let get_stats_counters t =
    t.stats

  let reset_stats_counters t =
    t.stats.rx_bytes <- 0L;
    t.stats.rx_pkts  <- 0l;
    t.stats.tx_bytes <- 0L;
    t.stats.tx_pkts  <- 0l

end
