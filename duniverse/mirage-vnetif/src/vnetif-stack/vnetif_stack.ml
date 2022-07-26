(*
 * Copyright (c) 2015-2020 Magnus Skjegstad <magnus@skjegstad.com>
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

module type Vnetif_stack =
sig
  type backend
  type buffer
  type id
  module V4 : Tcpip.Stack.V4
  module Backend : Vnetif.BACKEND

  (** Create a new IPv4 stack connected to an existing backend *)
  val create_stack_ipv4 : sw:Eio.Switch.t -> clock:Eio.Time.clock -> cidr:Ipaddr.V4.Prefix.t ->
    ?gateway:Ipaddr.V4.t -> ?mtu:int -> ?monitor_fn:(buffer -> unit) ->
    ?unlock_on_listen:Eio.Mutex.t ->
    backend -> V4.t
end

module Vnetif_stack (B : Vnetif.BACKEND)(R : Mirage_random.S)(Mclock : Mirage_clock.MCLOCK):
          Vnetif_stack with type backend = B.t =
struct
  type backend = B.t
  type buffer = B.buffer
  type id = B.id

  module Backend = B
  module V = Vnetif.Make(Backend)
  module E = Ethernet.Make(V)
  module A = Arp.Make(E)
  module Ip = Static_ipv4.Make(R)(Mclock)(E)(A)
  module Icmp = Icmpv4.Make(Ip)
  module U = Udp.Make(Ip)(R)
  module T = Tcp.Flow.Make(Ip)(Mclock)(R)
  module V4 = Tcpip_stack_direct.Make(R)(V)(E)(A)(Ip)(Icmp)(U)(T)

  let create_stack_ipv4 ~sw ~clock ~cidr ?gateway ?mtu ?monitor_fn ?unlock_on_listen backend =
    let netif = V.connect ?size_limit:mtu ?monitor_fn ?unlock_on_listen backend in
    let ethif = E.connect netif in
    let arp = A.connect ~sw ~clock ethif  in
    let ipv4 = Ip.connect ~cidr ?gateway ethif arp in
    let icmp = Icmp.connect ipv4 in
    let udp = U.connect ipv4 in
    let tcp = T.connect ~sw ~clock ipv4 in
    V4.connect ~sw netif ethif arp ipv4 icmp udp tcp
end

(*module Vnetif_stack_unix(B: Vnetif.BACKEND)(R : Mirage_random.S):
  Vnetif_stack with type backend = B.t =
struct
  module X = Vnetif_stack(B)(R)(Time)(Mclock)
  include X
end*)
