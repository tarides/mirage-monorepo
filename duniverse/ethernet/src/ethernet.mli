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
  (** Ethernet protocols. *)
  type proto = [ `ARP | `IPv4 | `IPv6 ]

  (** [pp_proto ppf proto] pretty-prints the ethernet protocol [proto] on [ppf]. *)
  val pp_proto: proto Fmt.t

  (** The type of an Ethernet packet. *)
  type t = {
    source : Macaddr.t;
    destination : Macaddr.t;
    ethertype : proto;
  }

  (** [sizeof_ethernet] is the byte size of the ethernet header. *)
  val sizeof_ethernet : int

  (** [of_cstruct buffer] attempts to decode the buffer as ethernet packet. It
      may result an error if the buffer is too small, or the protocol is not
      supported. *)
  val of_cstruct : Cstruct.t -> (t * Cstruct.t, string) result

  (** [into_cstruct t cs] attempts to encode the ethernet packet [t] into the
      buffer [cs] (at offset 0). This may fail if the buffer is not big
      enough. *)
  val into_cstruct : t -> Cstruct.t -> (unit, string) result

  (** [make_cstruct t] encodes the ethernet packet [t] into a freshly allocated
      buffer. *)
  val make_cstruct : t -> Cstruct.t
end

module type S = sig

  type nonrec error = private [> `Exceeds_mtu ]
  (** The type for ethernet interface errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type t
  (** The type representing the internal state of the ethernet layer. *)

  val disconnect: t -> unit Lwt.t
  (** Disconnect from the ethernet layer. While this might take some time to
      complete, it can never result in an error. *)

  val write: t -> ?src:Macaddr.t -> Macaddr.t -> Packet.proto -> ?size:int ->
    (Cstruct.t -> int) -> (unit, error) result Lwt.t
  (** [write eth ~src dst proto ~size payload] outputs an ethernet frame which
     header is filled by [eth], and its payload is the buffer from the call to
     [payload]. [Payload] gets a buffer of [size] (defaults to mtu) to fill with
     their payload. If [size] exceeds {!mtu}, an error is returned. *)

  val mac: t -> Macaddr.t
  (** [mac eth] is the MAC address of [eth]. *)

  val mtu: t -> int
  (** [mtu eth] is the Maximum Transmission Unit of the [eth] i.e. the maximum
      size of the payload, excluding the ethernet frame header. *)

  val input:
    arpv4:(Cstruct.t -> unit Lwt.t) ->
    ipv4:(Cstruct.t -> unit Lwt.t) ->
    ipv6:(Cstruct.t -> unit Lwt.t) ->
    t -> Cstruct.t -> unit Lwt.t
  (** [input ~arpv4 ~ipv4 ~ipv6 eth buffer] decodes the buffer and demultiplexes
      it depending on the protocol to the callback. *)
end

module Make (N : Mirage_net.S) : sig
  include S

  val connect : N.t -> t Lwt.t
  (** [connect netif] connects an ethernet layer on top of the raw
      network device [netif]. *)
end
