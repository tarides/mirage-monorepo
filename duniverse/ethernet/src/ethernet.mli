(*
 * Copyright (c) 2010-2019 Anil Madhavapeddy <anil@recoil.org>
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

(** OCaml Ethernet (IEEE 802.3) layer *)

(** Ethernet (IEEE 802.3) is a widely used data link layer. The hardware is
   usually a twisted pair or fibre connection, on the software side it consists
   of an Ethernet header where source and destination mac addresses, and a type
   field, indicating the type of the next layer, are present. The Ethernet layer
   consists of network card mac address and MTU information, and provides
   decapsulation and encapsulation. *)

(** {2 Ethernet layer} *)

module Packet : sig
  type proto = [ `ARP | `IPv4 | `IPv6 ]
  (** Ethernet protocols. *)

  val pp_proto : proto Fmt.t
  (** [pp_proto ppf proto] pretty-prints the ethernet protocol [proto] on [ppf]. *)

  type t = { source : Macaddr.t; destination : Macaddr.t; ethertype : proto }
  (** The type of an Ethernet packet. *)

  val sizeof_ethernet : int
  (** [sizeof_ethernet] is the byte size of the ethernet header. *)

  val of_cstruct : Cstruct.t -> (t * Cstruct.t, string) result
  (** [of_cstruct buffer] attempts to decode the buffer as ethernet packet. It
      may result an error if the buffer is too small, or the protocol is not
      supported. *)

  val into_cstruct : t -> Cstruct.t -> (unit, string) result
  (** [into_cstruct t cs] attempts to encode the ethernet packet [t] into the
      buffer [cs] (at offset 0). This may fail if the buffer is not big
      enough. *)

  val make_cstruct : t -> Cstruct.t
  (** [make_cstruct t] encodes the ethernet packet [t] into a freshly allocated
      buffer. *)
end

exception Exceeds_mtu  (** The type for ethernet interface errors. *)

type ethernet_sink = < 
  copy: ?src:Macaddr.t -> Macaddr.t -> Packet.proto -> Eio.Flow.source -> unit
>

type t = <
  ethernet_sink;
  arpv4: Eio.Flow.source;
  ipv6: Eio.Flow.source;
  ipv4: Eio.Flow.source;
  mac: Macaddr.t;
  (** [mac eth] is the MAC address of [eth]. *)
  mtu: int;
  (** [mtu eth] is the Maximum Transmission Unit of the [eth] i.e. the maximum
      size of the payload, excluding the ethernet frame header. *)
>

val copy : < ethernet_sink; .. > -> ?src:Macaddr.t -> Macaddr.t -> Packet.proto -> Eio.Flow.source -> unit

val mtu : < mtu: int; .. > -> int

val mac : < mac: Macaddr.t; .. > -> Macaddr.t

val connect : Mirage_net.t -> t
(** [connect netif] connects an ethernet layer on top of the raw
    network device [netif]. *)
