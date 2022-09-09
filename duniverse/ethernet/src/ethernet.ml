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

type ethernet_sink = < 
  copy: ?src:Macaddr.t -> Macaddr.t -> Packet.proto -> Eio.Flow.source -> unit
>

type t = <
  ethernet_sink;
  arpv4: Eio.Flow.source;
  ipv6: Eio.Flow.source;
  ipv4: Eio.Flow.source;
  mac: Macaddr.t;
  mtu: int;
>

let src = Logs.Src.create "ethernet" ~doc:"Mirage Ethernet"

module Log = (val Logs.src_log src : Logs.LOG)

module Impl = struct
  type t = { netif : Mirage_net.t; }

  let mac t = t.netif#mac
  let mtu t = t.netif#mtu (* interface MTU excludes Ethernet header *)

  let read ~arpv4 ~ipv4 ~ipv6 t =
    let open Ethernet_packet in
    MProf.Trace.label "ethernet.input";
    let frame = Cstruct.create (mtu t) in
    let _ = Eio.Flow.read t.netif frame in
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
    | Ok () -> 
      Eio.Flow.copy (Eio.Flow.cstruct_source (header_buffer::payload)) t.netif 

  let connect netif =
    MProf.Trace.label "ethernet.connect";
    let t = { netif } in
    Log.info (fun f -> f "Connected Ethernet interface %a" Macaddr.pp (mac t));
    t

  let disconnect t =
    Log.info (fun f ->
        f "Disconnected Ethernet interface %a" Macaddr.pp (mac t))
end


(* todo: don't allocate the same buffer ? *)
let chunk_cs = Cstruct.create 10000

let fallback_copy source flow ?src dst proto = 
  try
    while true do
      let got = Eio.Flow.read source chunk_cs in
      Impl.writev flow ?src dst proto [Cstruct.sub chunk_cs 0 got]
    done
  with End_of_file -> ()

let copy_with_rsb rsb flow ?src dst proto =
  try
    rsb @@ fun cstruct ->
    Impl.writev flow ?src dst proto cstruct;
    Cstruct.lenv cstruct
  with
  | End_of_file -> ()


let connect netif =
  let v = Impl.connect netif in
  
  let arpv4_packet, arpv4_condition = 
    ref None, Eio.Condition.create () 
  in
  let ipv4_packet, ipv4_condition = 
    ref None, Eio.Condition.create ()
  in
  let ipv6_packet, ipv6_condition = 
    ref None, Eio.Condition.create () 
  in

  let do_read () =
    (* todo: multicore ? *)
    Impl.read 
      ~arpv4:(fun pkt -> 
        arpv4_packet := Some pkt;
        Eio.Condition.broadcast arpv4_condition)
      ~ipv4:(fun pkt -> 
        ipv4_packet := Some pkt;
        Eio.Condition.broadcast ipv4_condition)
      ~ipv6:(fun pkt -> 
        ipv6_packet := Some pkt;
        Eio.Condition.broadcast ipv6_condition)
      v
  in

  let read ethertype =
    let pkt, cnd = match ethertype with
      | `ARP -> arpv4_packet, arpv4_condition
      | `IPv4 -> ipv4_packet, ipv4_condition
      | `IPv6 -> ipv6_packet, ipv6_condition
    in
    object (self: Eio.Flow.source)
      method probe _ = None

      method read_into cstruct =
        let rec rd ()= 
          match !pkt with
          | None ->
            Eio.Fiber.first 
              (fun () -> Eio.Condition.await_no_mutex arpv4_condition)
              (fun () -> do_read ());
            rd ()
          | Some v ->
            let buf = v in
            pkt := None;
            buf
        in
        let buf = rd () in
        let l = min (Cstruct.length buf) (Cstruct.length cstruct) in
        Cstruct.blit buf 0 cstruct 0 l;
        l

      method read_methods = []
    end

  in
  let arpv4 = read `ARP in
  let ipv4 = read `IPv4 in
  let ipv6 = read `IPv6
  in
  object (self: t)
    method mac = Impl.mac v
    method mtu = Impl.mtu v

    method copy ?src dst proto (source : #Eio.Flow.source) =
      let rec aux = function
        | Eio.Flow.Read_source_buffer rsb :: _ -> 
            copy_with_rsb rsb v ?src dst proto
        | _ :: xs -> aux xs
        | [] -> fallback_copy source v ?src dst proto
      in
      aux (Eio.Flow.read_methods source)

    method arpv4 = arpv4

    method ipv4 = ipv4

    method ipv6 = ipv6
  end

  let copy t = t#copy

  let mac t = t#mac
  
  let mtu t = t#mtu