(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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

(** {2 ARP} *)

exception Timeout
(** The type for ARP errors. *)

(** Address resolution protocol, translating network addresses (e.g. IPv4)
    into link layer addresses (MAC). *)
(* module type S = sig
  type t
  (** The type representing the internal state of the ARP layer. *)

  val disconnect: t -> unit
  (** Disconnect from the ARP layer. While this might take some time to
      complete, it can never result in an error. *)

  (** Prettyprint cache contents *)
  val pp : t Fmt.t

  (** [get_ips arp] gets the bound IP address list in the [arp]
      value. *)
  val get_ips : t -> Ipaddr.V4.t list

  (** [set_ips arp] sets the bound IP address list, which will transmit a
      GARP packet also. *)
  val set_ips : t -> Ipaddr.V4.t list -> unit

  (** [remove_ip arp ip] removes [ip] to the bound IP address list in
      the [arp] value, which will transmit a GARP packet for any remaining IPs in
      the bound IP address list after the removal. *)
  val remove_ip : t -> Ipaddr.V4.t -> unit

  (** [add_ip arp ip] adds [ip] to the bound IP address list in the
      [arp] value, which will transmit a GARP packet also. *)
  val add_ip : t -> Ipaddr.V4.t -> unit

  (** [query arp ip] queries the cache in [arp] for an ARP entry
      corresponding to [ip], which may result in the sender sleeping
      waiting for a response. *)
  val query : t -> Ipaddr.V4.t -> Macaddr.t

  (** [input arp frame] will handle an ARP frame. If it is a response,
      it will update its cache, otherwise will try to satisfy the
      request. *)
  val input : t -> Cstruct.t -> unit
end *)

type t = <
    pp: unit Fmt.t;
    get_ips: Ipaddr.V4.t list;
    set_ips: Ipaddr.V4.t list -> unit;
    remove_ip: Ipaddr.V4.t -> unit;
    add_ip: Ipaddr.V4.t -> unit;
    query: Ipaddr.V4.t -> Macaddr.t;
    Eio.Flow.sink;
>

val query : < query: Ipaddr.V4.t -> Macaddr.t; ..> -> Ipaddr.V4.t -> Macaddr.t

val connect : sw:Eio.Switch.t -> clock:Eio.Time.clock -> Ethernet.t -> t
