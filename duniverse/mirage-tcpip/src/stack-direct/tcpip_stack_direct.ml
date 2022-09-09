(*
 * Copyright (c) 2011-2014 Anil Madhavapeddy <anil@recoil.org>
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

let src = Logs.Src.create "tcpip-stack-direct" ~doc:"Pure OCaml TCP/IP stack"

module Log = (val Logs.src_log src : Logs.LOG)

module IPV4V6
    (Ipv4 : Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t)
    (Ipv6 : Tcpip.Ip.S with type ipaddr = Ipaddr.V6.t) =
struct
  type ipaddr = Ipaddr.t
  type callback = src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit

  let pp_ipaddr = Ipaddr.pp

  exception V4V6 of string
  type t = { ipv4 : Ipv4.t; ipv4_only : bool; ipv6 : Ipv6.t; ipv6_only : bool }

  let connect ~ipv4_only ~ipv6_only ipv4 ipv6 =
    if ipv4_only && ipv6_only then
      failwith "cannot configure stack with both IPv4 only and IPv6 only"
    else { ipv4; ipv4_only; ipv6; ipv6_only }

  let disconnect _ = ()

  let input t ~tcp ~udp ~default =
    let tcp4 ~src ~dst payload =
      tcp ~src:(Ipaddr.V4 src) ~dst:(Ipaddr.V4 dst) payload
    and tcp6 ~src ~dst payload =
      tcp ~src:(Ipaddr.V6 src) ~dst:(Ipaddr.V6 dst) payload
    and udp4 ~src ~dst payload =
      udp ~src:(Ipaddr.V4 src) ~dst:(Ipaddr.V4 dst) payload
    and udp6 ~src ~dst payload =
      udp ~src:(Ipaddr.V6 src) ~dst:(Ipaddr.V6 dst) payload
    and default4 ~proto ~src ~dst payload =
      default ~proto ~src:(Ipaddr.V4 src) ~dst:(Ipaddr.V4 dst) payload
    and default6 ~proto ~src ~dst payload =
      default ~proto ~src:(Ipaddr.V6 src) ~dst:(Ipaddr.V6 dst) payload
    in
    fun buf ->
      if Cstruct.length buf >= 1 then
        let v = Cstruct.get_uint8 buf 0 lsr 4 in
        if v = 4 && not t.ipv6_only then
          Ipv4.input t.ipv4 ~tcp:tcp4 ~udp:udp4 ~default:default4 buf
        else if v = 6 && not t.ipv4_only then
          Ipv6.input t.ipv6 ~tcp:tcp6 ~udp:udp6 ~default:default6 buf
        else ()
      else ()

  let write t ?fragment ?ttl ?src dst proto ?size headerf bufs =
    match dst with
    | Ipaddr.V4 dst ->
        if not t.ipv6_only then
          let src =
            match src with
            | None -> None
            | Some (Ipaddr.V4 src) -> (Some src)
            | _ -> raise (V4V6 "source must be V4 if dst is V4")
          in
          Ipv4.write t.ipv4 ?fragment ?ttl ?src dst proto ?size headerf bufs
        else (
          Log.warn (fun m ->
              m "attempted to write an IPv4 packet in a v6 only stack"))
    | Ipaddr.V6 dst ->
        if not t.ipv4_only then
          let src = 
            match src with
            | None -> None
            | Some (Ipaddr.V6 src) -> (Some src)
            | _ -> raise (V4V6 "source must be V6 if dst is V6")
          in
          Ipv6.write t.ipv6 ?fragment ?ttl ?src dst proto ?size headerf bufs
        else (
          Log.warn (fun m ->
              m "attempted to write an IPv6 packet in a v4 only stack"))

  let pseudoheader t ?src dst proto len =
    match dst with
    | Ipaddr.V4 dst ->
        let src =
          match src with
          | None -> None
          | Some (Ipaddr.V4 src) -> Some src
          | _ -> None (* cannot happen *)
        in
        Ipv4.pseudoheader t.ipv4 ?src dst proto len
    | Ipaddr.V6 dst ->
        let src =
          match src with
          | None -> None
          | Some (Ipaddr.V6 src) -> Some src
          | _ -> None (* cannot happen *)
        in
        Ipv6.pseudoheader t.ipv6 ?src dst proto len

  let src t ~dst =
    match dst with
    | Ipaddr.V4 dst -> Ipaddr.V4 (Ipv4.src t.ipv4 ~dst)
    | Ipaddr.V6 dst -> Ipaddr.V6 (Ipv6.src t.ipv6 ~dst)

  let get_ip t =
    List.map (fun ip -> Ipaddr.V4 ip) (Ipv4.get_ip t.ipv4)
    @ List.map (fun ip -> Ipaddr.V6 ip) (Ipv6.get_ip t.ipv6)

  let mtu t ~dst =
    match dst with
    | Ipaddr.V4 dst -> Ipv4.mtu t.ipv4 ~dst
    | Ipaddr.V6 dst -> Ipv6.mtu t.ipv6 ~dst
end

module MakeV4V6
    (Random : Mirage_random.S)
    (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.t)
    (Icmpv4 : Icmpv4.S)
    (Udp : Tcpip.Udp.S with type ipaddr = Ipaddr.t)
    (Tcp : Tcpip.Tcp.S with type ipaddr = Ipaddr.t) =
struct
  module UDP = Udp
  module TCP = Tcp
  module IP = Ip

  type t = {
    netif : Mirage_net.t;
    ethif : Ethernet.t;
    arpv4 : Arp.t;
    icmpv4 : Icmpv4.t;
    ip : IP.t;
    udp : Udp.t;
    tcp : Tcp.t;
    cancel : unit Eio.Promise.u;
  }

  let pp fmt t =
    Format.fprintf fmt "mac=%a,ip=%a" Macaddr.pp (Ethernet.mac t.ethif)
      (Fmt.list Ipaddr.pp) (IP.get_ip t.ip)

  let tcp { tcp; _ } = tcp
  let udp { udp; _ } = udp
  let ip { ip; _ } = ip

  let listen t =
    Log.debug (fun f -> f "Establishing or updating listener for stack %a" pp t);
    let tcp ~src ~dst buf = 
      Tcp.input t.tcp ~src ~dst buf
    and udp = Udp.input t.udp
    and default ~proto ~src ~dst buf =
      Log.app (fun f -> f "PROTO %d" proto);
      match (proto, src, dst) with
      | 1, Ipaddr.V4 src, Ipaddr.V4 dst -> Icmpv4.input t.icmpv4 ~src ~dst buf
      | _ -> ()
    in
    let ipv4_listen () =
      let ipv4_buffer = Cstruct.create (Ethernet.mtu t.ethif) in
      while true do
        let sz = Eio.Flow.read t.ethif#ipv4 ipv4_buffer in
        IP.input ~tcp ~udp ~default t.ip (Cstruct.sub ipv4_buffer 0 sz)
      done
    in
    let ipv6_listen () =
      let ipv6_buffer = Cstruct.create (Ethernet.mtu t.ethif) in
      while true do
        let sz = Eio.Flow.read t.ethif#ipv6 ipv6_buffer in
        IP.input ~tcp ~udp ~default t.ip (Cstruct.sub ipv6_buffer 0 sz)
      done
    in
    let arp_listen () =
      Eio.Flow.copy t.ethif#arpv4 t.arpv4
    in
    let listeners =
      List.init 4 (fun _ () -> 
        Eio.Switch.run @@ fun sw -> 
        Eio.Fiber.all [
          ipv4_listen;
          ipv6_listen;
          arp_listen
        ]) 
    in
    Eio.Fiber.any listeners;
    let nstat = t.netif#get_stats_counters in
    let open Mirage_net in
    Log.info (fun f ->
        f
          "listening loop of interface %s terminated regularly:@ %Lu bytes \
            (%lu packets) received, %Lu bytes (%lu packets) sent@ "
          (Macaddr.to_string (t.netif#mac))
          nstat.rx_bytes nstat.rx_pkts nstat.tx_bytes nstat.tx_pkts)

  let connect ~sw netif ethif arpv4 ip icmpv4 udp tcp =
    let cancel_promise, cancel = Eio.Promise.create ~label:"tcpip.stack-direct.connect" () in
    let t = { netif; ethif; arpv4; ip; icmpv4; tcp; udp; cancel } in
    Log.info (fun f -> f "stack assembled: %a" pp t);
    Eio.Fiber.fork ~sw (fun () ->
        Eio.Private.Ctf.label "tcpip.stack-direct.listen";
        Eio.Fiber.first
          (fun () -> 
            Eio.Private.Ctf.label "tcpip.stack-direct.listen~network";
            listen t)
          (fun () -> 
            Eio.Private.Ctf.label "tcpip.stack-direct.listen~cancel";
            Eio.Promise.await cancel_promise));
    t

  let disconnect t =
    Log.info (fun f -> f "disconnect called: %a" pp t);
    Eio.Promise.resolve t.cancel ()
end
