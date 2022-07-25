(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2016 Hannes Mehnert <hannes@mehnert.org>
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
 *
 *)

module type S = sig
  type t
  val disconnect: t -> unit
  val pp : t Fmt.t
  val get_ips : t -> Ipaddr.V4.t list
  val set_ips : t -> Ipaddr.V4.t list -> unit
  val remove_ip : t -> Ipaddr.V4.t -> unit
  val add_ip : t -> Ipaddr.V4.t -> unit
  val query : t -> Ipaddr.V4.t -> Macaddr.t
  val input : t -> Cstruct.t -> unit
end

let logsrc = Logs.Src.create "ARP" ~doc:"Mirage ARP handler"

exception Timeout

type 'a or_exn = ('a, exn) result

module Make (Ethernet : Ethernet.S) = struct

  open Eio.Std

  type t = {
    mutable state : (Macaddr.t or_exn Promise.t * Macaddr.t or_exn Promise.u) Arp_handler.t ;
    ethif : Ethernet.t ;
    mutable ticking : bool ;
    clock : Eio.Time.clock ;
  }

  let probe_repeat_delay = Duration.of_ms 1500 (* per rfc5227, 2s >= probe_repeat_delay >= 1s *)

  let output t (arp, destination) =
    let size = Arp_packet.size in
    let buf = Cstruct.create_unsafe size in
    Arp_packet.encode_into arp buf;
    try 
      Ethernet.writev t.ethif destination `ARP [buf]
    with
    | e ->
      Logs.warn ~src:logsrc
        (fun m -> m "exception %s while outputting packet %a to %a"
            (Printexc.to_string e) Arp_packet.pp arp Macaddr.pp destination)

  let rec tick t () =
    if t.ticking then begin
      Eio.Time.sleep t.clock (Duration.to_f probe_repeat_delay );
      let state, requests, timeouts = Arp_handler.tick t.state in
      t.state <- state ;
      List.map (fun r () -> output t r) requests
      |> Fiber.all;
      List.iter (fun (_, u) -> Promise.resolve u (Error Timeout)) timeouts ;
      tick t ()
    end

  let pp ppf t = Arp_handler.pp ppf t.state

  let input t frame =
    let state, out, wake = Arp_handler.input t.state frame in
    t.state <- state ;
    (match out with
     | None -> ()
     | Some pkt -> output t pkt) ;
    match wake with
    | None -> ()
    | Some (mac, (_, u)) -> 
      Promise.resolve u (Ok mac)

  let get_ips t = Arp_handler.ips t.state

  let create ?ipaddr t =
    let mac = Arp_handler.mac t.state in
    let state, out = Arp_handler.create ~logsrc ?ipaddr mac in
    t.state <- state ;
    match out with
    | None -> ()
    | Some x -> output t x

  let add_ip t ipaddr =
    match Arp_handler.ips t.state with
    | [] -> create ~ipaddr t
    | _ ->
      let state, out, wake = Arp_handler.alias t.state ipaddr in
      t.state <- state ;
      output t out ;
      match wake with
      | None -> ()
      | Some (_, u) -> Promise.resolve u (Ok (Arp_handler.mac t.state))

  let init_empty mac =
    let state, _ = Arp_handler.create ~logsrc mac in
    state

  let set_ips t = function
    | [] ->
      let mac = Arp_handler.mac t.state in
      let state = init_empty mac in
      t.state <- state
    | ipaddr::xs ->
      create ~ipaddr t ;
      List.iter (add_ip t) xs

  let remove_ip t ip =
    let state = Arp_handler.remove t.state ip in
    t.state <- state 

  let query t ip =
    let merge = function
      | None -> 
        Promise.create ~label:"ARP response" ()
      | Some a -> a
    in
    let state, res = Arp_handler.query t.state ip merge in
    t.state <- state ;
    match res with
    | Arp_handler.RequestWait (pkt, (tr, _)) -> 
      (output t pkt;
      match 
        Promise.await tr
      with
      | Ok v -> v
      | Error exn -> raise exn)
    | Arp_handler.Wait (t, _) -> 
      (match 
        Promise.await t
      with
      | Ok v -> v
      | Error exn -> raise exn)
    | Arp_handler.Mac m -> 
      m

  let connect ~sw ~clock ethif =
    let mac = Ethernet.mac ethif in
    let state = init_empty mac in
    let t = { ethif; state; ticking = true; clock} in
    Fiber.fork ~sw (tick t);
    t

  let disconnect t =
    t.ticking <- false
end
