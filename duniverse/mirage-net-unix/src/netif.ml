(*
 * Copyright (c) 2010-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2015      Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Eio.Std

let src = Logs.Src.create "netif" ~doc:"Mirage unix network module"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  id : string;
  dev : Eio_linux.FD.t;
  mutable active : bool;
  mac : Macaddr.t;
  mtu : int;
  stats : Mirage_net.stats;
  output_buffer : Cstruct.t;
  output_region : Uring.Region.t;
}

let fd t = t.dev

exception (*Partial of string * int * Cstruct.t*) Disconnected


let err_permission_denied devname =
  Printf.sprintf
    "Permission denied while opening the %s device. Please re-run using sudo."
    devname

let connect ~sw devname =
  try
    Random.self_init ();
    let fd, devname = Tuntap.opentap ~pi:false ~devname () in
    let dev = Eio_linux.FD.of_unix ~sw ~seekable:false ~close_unix:false fd in
    let mac = Macaddr.make_local (fun _ -> Random.int 256) in
    Tuntap.set_up_and_running devname;
    let mtu = Tuntap.get_mtu devname in
    Log.debug (fun m ->
        m "plugging into %s with mac %a and mtu %d" devname Macaddr.pp mac mtu);
    let active = true in
    let stats = Mirage_net.Stats.create () in
    let slots = 64 in
    let output_buffer = Cstruct.create (mtu * slots) in
    let t =
      {
        id = devname;
        dev;
        active;
        mac;
        mtu;
        stats;
        output_buffer;
        output_region =
          Uring.Region.init ~block_size:mtu output_buffer.buffer slots;
      }
    in
    Log.info (fun m -> m "connect %s with mac %a" devname Macaddr.pp mac);
    t
  with
  | ((Failure "tun[open]: Permission denied") [@warning "-52"]) ->
      failwith (err_permission_denied devname)
  | exn -> raise exn

let disconnect t =
  Log.info (fun m -> m "disconnect %s" t.id);
  t.active <- false;
  Eio_linux.FD.close t.dev

(* Input a frame, and block if nothing is available *)
let rec read ~upto t buf =
  let process () =
    Eio.Private.Ctf.note_increase "net_read" 1;
    let v = Eio_linux.Low_level.read_upto t.dev buf upto in
    Eio.Private.Ctf.note_increase "net_read" (-1);
    match v with
    | -1 -> Error `Continue (* EAGAIN or EWOULDBLOCK *)
    | 0 -> Error `Disconnected (* EOF *)
    | len ->
        Mirage_net.Stats.rx t.stats (Int64.of_int len);
        let buf = Uring.Region.to_cstruct ~len buf in
        Ok buf
    | exception Unix.Unix_error (Unix.ENXIO, _, _) ->
        Log.err (fun m -> m "[read] device %s is down, stopping" t.id);
        Error `Disconnected
    | exception exn ->
        Log.err (fun m ->
            m "[read] error: %s, continuing" (Printexc.to_string exn));
        Error `Continue
  in
  match process () with
  | Error `Continue -> read ~upto t buf
  | Error `Disconnected -> Error `Disconnected
  | Ok buf -> Ok buf

let safe_apply f x =
  try f x with
  | Out_of_memory -> raise Out_of_memory
  | exn ->
      Log.err (fun m ->
          m "[listen] error while handling %s, continuing. bt: %s"
            (Printexc.to_string exn)
            (Printexc.get_backtrace ()))



(* Loop and listen for packets permanently *)
(* this function has to be tail recursive, since it is called at the
   top level, otherwise memory of received packets and all reachable
   data is never claimed.  take care when modifying, here be dragons! *)
let listen t ~header_size fn =
  let listeners = 
    List.init 8 (fun _ () ->
    Switch.run @@ fun sw ->
    let rec loop () =
      match t.active with
      | true -> (
          let region = Eio_linux.Low_level.alloc_fixed_or_wait () in
          let process () =
            match read ~upto:(t.mtu + header_size) t region with
            | Ok buf ->
                Fiber.fork ~sw (fun () ->
                  Log.info (fun f -> f "netif: read (%d)" (Cstruct.length buf));
                  safe_apply fn buf;
                  Eio_linux.Low_level.free_fixed region)
            | Error `Canceled -> raise Disconnected
            | Error `Disconnected ->
                t.active <- false;
                raise Disconnected
          in
          process ();
          (loop [@tailcall]) ())
      | false -> ()
    in
    loop ())
  in
  Fiber.any listeners

(* Transmit a packet from a Cstruct.t *)
let writev t bufs =
  Log.info (fun f -> f "netif: writev (%d)" (Cstruct.lenv bufs));
  Eio.Private.Ctf.label "netif: writev";
  Eio_linux.Low_level.writev t.dev bufs;
  Mirage_net.Stats.tx t.stats (Int64.of_int (Cstruct.lenv bufs))

let mac t = t.mac
let mtu t = t.mtu
let get_stats_counters t = t.stats
let reset_stats_counters t = Mirage_net.Stats.reset t.stats
