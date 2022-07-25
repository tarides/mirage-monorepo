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


(* TODO Some of these modules and signatures could eventually be moved
   to mirage-vnetif *)
module Clock = Mclock

module type VNETIF_STACK = sig
  type backend
  type buffer
  type id

  module Stackv4 : Tcpip.Stack.V4
  module Stackv6 : Tcpip.Stack.V6

  val create_backend :
    sw:Eio.Switch.t -> clock:Eio.Time.clock -> unit -> backend
  (** Create a new backend *)

  val create_stack :
    sw:Eio.Switch.t ->
    clock:Eio.Time.clock ->
    ?mtu:int ->
    cidr:Ipaddr.V4.Prefix.t ->
    ?gateway:Ipaddr.V4.t ->
    backend ->
    Stackv4.t
  (** Create a new stack connected to an existing backend *)

  val create_stack_v6 :
    sw:Eio.Switch.t ->
    clock:Eio.Time.clock ->
    ?mtu:int ->
    ?cidr:Ipaddr.V6.Prefix.t ->
    ?gateway:Ipaddr.V6.t ->
    backend ->
    Stackv6.t
  (** [create_stack ?mtu ?cidr ?gateway backend] adds a listener
      function to the backend *)

  val create_backend_listener : backend -> (buffer -> unit) -> id

  val disable_backend_listener : backend -> id -> unit
  (** Disable a listener function *)

  val record_pcap : dir:Eio.Dir.t -> backend -> string -> (unit -> unit) -> unit
  (** Records pcap data from the backend while running the specified
      function. Disables the pcap recorder when the function exits. *)
end

module VNETIF_STACK (B : Vnetif_backends.Backend) :
  VNETIF_STACK with type backend = B.t = struct
  type backend = B.t
  type buffer = B.buffer
  type id = B.id

  module V = Vnetif.Make (B)
  module E = Ethernet.Make (V)
  module A = Arp.Make (E)
  module Ip4 = Static_ipv4.Make (Mirage_random_test) (Clock) (E) (A)
  module Icmp4 = Icmpv4.Make (Ip4)
  module U4 = Udp.Make (Ip4) (Mirage_random_test)
  module T4 = Tcp.Flow.Make (Ip4) (Clock) (Mirage_random_test)
  module Ip6 = Ipv6.Make (V) (E) (Mirage_random_test)
  module U6 = Udp.Make (Ip6) (Mirage_random_test)
  module T6 = Tcp.Flow.Make (Ip6) (Clock) (Mirage_random_test)

  module Stackv4 =
    Tcpip_stack_direct.Make (Mirage_random_test) (V) (E) (A) (Ip4) (Icmp4) (U4)
      (T4)

  module Stackv6 =
    Tcpip_stack_direct.MakeV6 (Mirage_random_test) (V) (E) (Ip6) (U6) (T6)

  let create_backend ~sw ~clock () = B.create ~sw ~clock ()

  let create_stack ~sw ~clock ?mtu ~cidr ?gateway backend =
    let size_limit = match mtu with None -> None | Some x -> Some x in
    let netif = V.connect ?size_limit backend in
    let ethif = E.connect netif in
    let arpv4 = A.connect ~sw ~clock ethif  in
    let ipv4 = Ip4.connect ~cidr ?gateway ethif arpv4 in
    let icmpv4 = Icmp4.connect ipv4 in
    let udpv4 = U4.connect ipv4 in
    let tcpv4 = T4.connect ~sw ~clock ipv4 in
    Stackv4.connect ~sw netif ethif arpv4 ipv4 icmpv4 udpv4 tcpv4

  let create_stack_v6 ~sw ~clock ?mtu ?cidr ?gateway backend =
    let size_limit = match mtu with None -> None | Some x -> Some x in
    let netif = V.connect ?size_limit backend in
    let ethif = E.connect netif in
    let ipv6 = Ip6.connect ?cidr ?gateway ~sw ~clock netif ethif in
    let udpv6 = U6.connect ipv6 in
    let tcpv6 = T6.connect ~sw ~clock ipv6 in
    Stackv6.connect ~sw netif ethif ipv6 udpv6 tcpv6

  let create_backend_listener backend listenf =
    let id = B.register backend in
    B.set_listen_fn backend id listenf;
    id

  let disable_backend_listener backend id = B.unregister_and_flush backend id

  let create_pcap_recorder backend channel =
    let header_buf = Cstruct.create Pcap.sizeof_pcap_header in
    Pcap.LE.set_pcap_header_magic_number header_buf Pcap.magic_number;
    Pcap.LE.set_pcap_header_network header_buf Pcap.Network.(to_int32 Ethernet);
    Pcap.LE.set_pcap_header_sigfigs header_buf 0l;
    Pcap.LE.set_pcap_header_snaplen header_buf 0xffffl;
    Pcap.LE.set_pcap_header_thiszone header_buf 0l;
    Pcap.LE.set_pcap_header_version_major header_buf Pcap.major_version;
    Pcap.LE.set_pcap_header_version_minor header_buf Pcap.minor_version;
    Eio.Flow.(copy (cstruct_source [ header_buf ])) channel;
    let mutex = Eio.Mutex.create ~label:"vnetif.pcap_recorder_mutex" () in
    let pcap_record channel buffer =
      Eio.Mutex.with_lock mutex @@ fun () ->
      let pcap_buf = Cstruct.create Pcap.sizeof_pcap_packet in
      let time = Unix.gettimeofday () in
      Pcap.LE.set_pcap_packet_incl_len pcap_buf
        (Int32.of_int (Cstruct.length buffer));
      Pcap.LE.set_pcap_packet_orig_len pcap_buf
        (Int32.of_int (Cstruct.length buffer));
      Pcap.LE.set_pcap_packet_ts_sec pcap_buf (Int32.of_float time);
      let frac = (time -. float_of_int (truncate time)) *. 1000000.0 in
      Pcap.LE.set_pcap_packet_ts_usec pcap_buf (Int32.of_float frac);
      try 
        Eio.Flow.(copy (cstruct_source [ pcap_buf; buffer ])) channel
      with End_of_file ->
        Printf.printf "Warning: Pcap output channel already closed.\n"
    in
    create_backend_listener backend (pcap_record channel)

  let record_pcap ~dir backend pcap_file fn =
    try
      Eio.Dir.with_open_out ~create:(`If_missing 0o666) dir pcap_file
      @@ fun oc ->
      let recorder_id = create_pcap_recorder backend oc in
      fn ();
      disable_backend_listener backend recorder_id
    with Unix.Unix_error _ ->
      Printf.printf
        "Could not create pcap file %s - something along the way doesn't exist.\n"
        pcap_file;
      fn ()
end
