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
  dev : Eio_unix.socket;
  mutable active : bool;
  mac : Macaddr.t;
  mtu : int;
  stats : Mirage_net.stats;
  read_buffer : Netif_region.t;
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
    let dev = Eio_unix.FD.as_socket ~sw ~close_unix:true fd in
    let mac = Macaddr.make_local (fun _ -> Random.int 256) in
    Tuntap.set_up_and_running devname;
    let mtu = Tuntap.get_mtu devname in
    Log.debug (fun m ->
        m "plugging into %s with mac %a and mtu %d" devname Macaddr.pp mac mtu);
    let active = true in
    let stats = Mirage_net.Stats.create () in
    let slots = 64 in
    let size = mtu + 18 (* TODO *) in
    let buffer = Cstruct.create (size * slots) in
    let read_buffer = Netif_region.init ~block_size:size buffer.buffer slots in
    let t =
      {
        id = devname;
        dev;
        active;
        mac;
        mtu;
        stats;
        read_buffer;
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
  Eio.Flow.close t.dev

(* Input a frame, and block if nothing is available *)
let rec read t buf =
  let process () =
    Eio.Private.Ctf.note_increase "net_read" 1;
    let v = match Eio.Flow.read t.dev buf with
    | -1 -> Error `Continue (* EAGAIN or EWOULDBLOCK *)
    | 0 -> Error `Disconnected (* EOF *)
    | len ->
        Mirage_net.Stats.rx t.stats (Int64.of_int len);
        Ok (Cstruct.sub buf 0 len)
    | exception Unix.Unix_error (Unix.ENXIO, _, _) ->
        Log.err (fun m -> m "[read] device %s is down, stopping" t.id);
        Error `Disconnected
    (* | exception exn ->
        Log.err (fun m ->
            m "[read] error: %s, continuing" (Printexc.to_string exn));
        Error `Continue  *)
    in
    Eio.Private.Ctf.note_increase "net_read" (-1);
    v
  in
  match process () with
  | Error `Continue -> read t buf
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
          let region = Cstruct.create (t.mtu + header_size) in
          let chunk = Netif_region.alloc_block t.read_buffer in
          let process () =
            match read t (Netif_region.to_cstruct chunk) with
            | Ok buf ->
                Fiber.fork ~sw (fun () ->
                  Log.debug (fun f -> f "netif: read (%d)" (Cstruct.length buf));
                  safe_apply fn buf;
                  Netif_region.free chunk)
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
  Log.debug (fun f -> f "netif: writev (%d)" (Cstruct.lenv bufs));
  Eio.Private.Ctf.label "netif: writev";
  Eio.Flow.copy (Eio.Flow.cstruct_source bufs) t.dev;
  Mirage_net.Stats.tx t.stats (Int64.of_int (Cstruct.lenv bufs))

let mac t = t.mac
let mtu t = t.mtu
let get_stats_counters t = t.stats
let reset_stats_counters t = Mirage_net.Stats.reset t.stats

(* Eio interop *)

(* todo: don't allocate the same buffer ? *)
let chunk_cs = Cstruct.create 10000

let fallback_copy src flow = 
  try
    while true do
      let got = Eio.Flow.read src chunk_cs in
      writev flow [Cstruct.sub chunk_cs 0 got]
    done
  with End_of_file -> ()

let copy_with_rsb rsb flow =
  try
    rsb @@ fun cstruct ->
    writev flow cstruct;
    Cstruct.lenv cstruct
  with
  | End_of_file -> ()

let connect ~sw t : Mirage_net.t =
  let v = connect ~sw t in
  object (self: Mirage_net.t)
    method copy (src : #Eio.Flow.source) =
      let rec aux = function
        | Eio.Flow.Read_source_buffer rsb :: _ -> copy_with_rsb rsb v
        | _ :: xs -> aux xs
        | [] -> fallback_copy src v
      in
      aux (Eio.Flow.read_methods src)

    method read_into cstruct =
      let cst = read v cstruct |> Result.get_ok in
      Cstruct.length cst

    method shutdown _ = ()

    method probe _ = None

    method read_methods = []

    method get_stats_counters = get_stats_counters v

    method mac = mac v

    method mtu = mtu v

    method reset_stats_counters  = reset_stats_counters v


  end