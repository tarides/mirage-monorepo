(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2011 Richard Mortier <richard.mortier@nottingham.ac.uk>
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

module Packet = struct
  type proto = Ethernet_packet.proto

  let pp_proto = Ethernet_packet.pp_proto

  type t = Ethernet_packet.t = {
    source : Macaddr.t;
    destination : Macaddr.t;
    ethertype : proto;
  }

  let sizeof_ethernet = Ethernet_wire.sizeof_ethernet
  let of_cstruct = Ethernet_packet.Unmarshal.of_cstruct
  let into_cstruct = Ethernet_packet.Marshal.into_cstruct
  let make_cstruct = Ethernet_packet.Marshal.make_cstruct
end

exception Exceeds_mtu

module type S = sig
  type t

  val disconnect : t -> unit

  val writev :
    t ->
    ?src:Macaddr.t ->
    Macaddr.t ->
    Packet.proto ->
    Cstruct.t list ->
    unit

  val mac : t -> Macaddr.t
  val mtu : t -> int

  val input :
    arpv4:(Cstruct.t -> unit) ->
    ipv4:(Cstruct.t -> unit) ->
    ipv6:(Cstruct.t -> unit) ->
    t ->
    Cstruct.t ->
    unit
end

let src = Logs.Src.create "ethernet" ~doc:"Mirage Ethernet"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Netif : Mirage_net.S) = struct
  type t = { netif : Netif.t; }

  let mac t = Netif.mac t.netif
  let mtu t = Netif.mtu t.netif (* interface MTU excludes Ethernet header *)

  let input ~arpv4 ~ipv4 ~ipv6 t frame =
    let open Ethernet_packet in
    MProf.Trace.label "ethernet.input";
    let of_interest dest =
      Macaddr.compare dest (mac t) = 0 || not (Macaddr.is_unicast dest)
    in
    match Unmarshal.of_cstruct frame with
    | Ok (header, payload) when of_interest header.destination -> (
        match header.Ethernet_packet.ethertype with
        | `ARP -> arpv4 payload
        | `IPv4 -> ipv4 payload
        | `IPv6 -> ipv6 payload)
    | Ok _ -> ()
    | Error s -> Log.debug (fun f -> f "dropping Ethernet frame: %s" s)

  let writev t ?src destination ethertype payload =
    MProf.Trace.label "ethernet.write";
    let source = match src with None -> mac t | Some x -> x
    and eth_hdr_size = Ethernet_wire.sizeof_ethernet
    and mtu = mtu t in
    if Cstruct.lenv payload > mtu then
      raise Exceeds_mtu;
    let hdr = { Ethernet_packet.source; destination; ethertype } in
    let header_buffer = Cstruct.create_unsafe eth_hdr_size in 
    match Ethernet_packet.Marshal.into_cstruct hdr header_buffer with
    | Error msg ->
        Log.err (fun m ->
            m
              "error %s while marshalling ethernet header into allocated \
                buffer"
              msg);
        failwith "todo"
    | Ok () -> Netif.writev t.netif (header_buffer::payload)

  let connect netif =
    MProf.Trace.label "ethernet.connect";
    let t = { netif } in
    Log.info (fun f -> f "Connected Ethernet interface %a" Macaddr.pp (mac t));
    t

  let disconnect t =
    Log.info (fun f ->
        f "Disconnected Ethernet interface %a" Macaddr.pp (mac t))
end
