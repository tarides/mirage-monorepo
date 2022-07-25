open Common
module B = Vnetif_backends.Basic
module V = Vnetif.Make(B)
module E = Ethernet.Make(V)

module Ipv6 = Ipv6.Make(V)(E)(Mirage_random_test)
module Udp = Udp.Make(Ipv6)(Mirage_random_test)

let ip =
  let module M = struct
    type t = Ipaddr.V6.t
    let pp = Ipaddr.V6.pp
    let equal p q = (Ipaddr.V6.compare p q) = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

type stack = {
  backend : B.t;
  netif : V.t;
  ethif : E.t;
  ip : Ipv6.t;
  udp : Udp.t
}

let get_stack ~sw ~clock backend address =
  let cidr = Ipaddr.V6.Prefix.make 64 address in
  let netif = V.connect backend in
  let ethif = E.connect netif in
  let ip = Ipv6.connect ~cidr ~sw ~clock netif ethif in
  let udp = Udp.connect ip in
  { backend; netif; ethif; ip; udp }

let noop = fun ~src:_ ~dst:_ _ -> ()

let listen ?(tcp = noop) ?(udp = noop) ?(default = noop) stack =
  V.listen stack.netif ~header_size:Ethernet.Packet.sizeof_ethernet
    ( E.input stack.ethif
      ~arpv4:ignore
      ~ipv4:ignore
      ~ipv6:(
        Ipv6.input stack.ip
          ~tcp:tcp
          ~udp:udp
          ~default:(fun ~proto:_ -> default))) |> ignore

let udp_message = Cstruct.of_string "hello on UDP over IPv6"

let check_for_one_udp_packet on_received_one ~src ~dst buf =
  (match Udp_packet.Unmarshal.of_cstruct buf with
  | Ok (_, payload) ->
    Alcotest.(check ip) "sender address" (Ipaddr.V6.of_string_exn "fc00::23") src;
    Alcotest.(check ip) "receiver address" (Ipaddr.V6.of_string_exn "fc00::45") dst;
    Alcotest.(check cstruct) "payload is correct" udp_message payload
  | Error m -> Alcotest.fail m);
  (try Eio.Promise.resolve on_received_one () with _ -> () (* the first succeeds, the rest raise *));
  ()

let send_forever ~clock sender receiver_address udp_message =
  let rec loop () =
    Udp.write sender.udp ~dst:receiver_address ~dst_port:1234 udp_message;
    Eio.Time.sleep clock 0.050;
    loop () in
  loop ()

let pass_udp_traffic ~sw ~env () =
  let clock = env#clock in
  let sender_address = Ipaddr.V6.of_string_exn "fc00::23" in
  let receiver_address = Ipaddr.V6.of_string_exn "fc00::45" in
  let backend = B.create ~sw ~clock () in
  let sender = get_stack ~sw ~clock backend sender_address in
  let receiver = get_stack ~sw ~clock backend receiver_address in
  let received_one, on_received_one = Eio.Promise.create () in
  Eio.Fiber.any [
    (fun () -> listen receiver ~udp:(check_for_one_udp_packet on_received_one));
    (fun () -> listen sender);
    (fun () -> send_forever ~clock sender receiver_address udp_message);
    (fun () -> Eio.Promise.await received_one); (* stop on the first packet *)
    (fun () -> 
      Eio.Time.sleep clock 3.;
      Alcotest.fail "UDP packet should have been received")
  ]

let create_ethernet backend =
  let netif = V.connect backend in
  let ethif = E.connect netif in
  (fun ipv6 ->
     V.listen netif ~header_size:Ethernet.Packet.sizeof_ethernet
       (E.input ethif
          ~arpv4:ignore
          ~ipv4:ignore
          ~ipv6) |> ignore),
  (fun dst iovec -> E.writev ethif dst `IPv6 iovec),
  E.mac ethif

let solicited_node_prefix =
  Ipaddr.V6.(Prefix.make 104 (of_int16 (0xff02, 0, 0, 0, 0, 1, 0xff00, 0)))

let dad_na_is_sent ~sw ~env () =
  let address = Ipaddr.V6.of_string_exn "fc00::23" in
  let clock = env#clock in
  let backend = B.create ~sw ~clock () in
  let stack = get_stack ~sw ~clock backend address in
  let (listen_raw, write_raw, _) = create_ethernet backend in
  let received_one, on_received_one = Eio.Promise.create () in
  let nd_size = Ipv6_wire.sizeof_ipv6 + Ipv6_wire.sizeof_ns in
  let nd buf =
    Ipv6_wire.set_ipv6_version_flow buf 0x60000000l; (* IPv6 *)
    Ipv6_wire.set_ipv6_len buf Ipv6_wire.sizeof_ns;
    Ipaddr_cstruct.V6.write_cstruct_exn Ipaddr.V6.unspecified (Cstruct.shift buf 8);
    Ipaddr_cstruct.V6.write_cstruct_exn (Ipaddr.V6.Prefix.network_address solicited_node_prefix address) (Cstruct.shift buf 24);
    Ipv6_wire.set_ipv6_hlim buf 255;
    Ipv6_wire.set_ipv6_nhdr buf (Ipv6_wire.protocol_to_int `ICMP);
    let hdr, icmpbuf = Cstruct.split buf Ipv6_wire.sizeof_ipv6 in
    Ipv6_wire.set_ns_ty icmpbuf 135; (* NS *)
    Ipv6_wire.set_ns_code icmpbuf 0;
    Ipv6_wire.set_ns_reserved icmpbuf 0l;
    Ipaddr_cstruct.V6.write_cstruct_exn address (Cstruct.shift icmpbuf 8);
    Ipv6_wire.set_icmpv6_csum icmpbuf 0;
    Ipv6_wire.set_icmpv6_csum icmpbuf @@ Ndpv6.checksum hdr [icmpbuf];
    nd_size
  and is_na buf =
    let icmpbuf = Cstruct.shift buf Ipv6_wire.sizeof_ipv6 in
    Ipv6_wire.get_ipv6_version_flow buf = 0x60000000l && (* IPv6 *)
    Ipaddr.V6.compare
      (Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.shift buf 8))
      address = 0 &&
    Ipaddr.V6.compare
      (Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.shift buf 24))
      Ipaddr.V6.link_nodes = 0 &&
    Ipv6_wire.get_ipv6_hlim buf = 255 &&
    Ipv6_wire.get_ipv6_nhdr buf = Ipv6_wire.protocol_to_int `ICMP &&
    Ipv6_wire.get_na_ty icmpbuf = 136 &&
    Ipv6_wire.get_na_code icmpbuf = 0 &&
    Ipaddr.V6.compare
      (Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.shift icmpbuf 8))
      address = 0
  in
  Eio.Fiber.any [
    (fun () -> listen stack);
    (fun () -> 
      listen_raw (fun buf ->
        if is_na buf then
          Eio.Promise.resolve on_received_one ()));
    (fun () -> 
      let buf = Cstruct.create nd_size in 
      let _ = nd buf in
      write_raw (E.mac stack.ethif) [buf] |> ignore);
    (fun () -> Eio.Promise.await received_one);
    (fun () -> 
      Eio.Time.sleep clock 1.;
      Alcotest.fail "NA packet should have been received")
  ]

let multicast_mac =
  let pbuf = Cstruct.create 6 in
  Cstruct.BE.set_uint16 pbuf 0 0x3333;
  fun ip ->
    let _, _, _, n = Ipaddr.V6.to_int32 ip in
    Cstruct.BE.set_uint32 pbuf 2 n;
    Macaddr_cstruct.of_cstruct_exn pbuf

let dad_na_is_received ~sw ~env () =
  let clock = env#clock in
  let address = Ipaddr.V6.of_string_exn "fc00::23" in
  let backend = B.create ~sw ~clock () in
  let (listen_raw, write_raw, mac) = create_ethernet backend in
  let na_size = Ipv6_wire.sizeof_ipv6 + Ipv6_wire.sizeof_na + Ipv6_wire.sizeof_llopt in
  let is_ns buf =
    let icmpbuf = Cstruct.shift buf Ipv6_wire.sizeof_ipv6 in
    if
      Ipv6_wire.get_ipv6_version_flow buf = 0x60000000l && (* IPv6 *)
      Ipaddr.V6.compare
        (Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.shift buf 8))
        Ipaddr.V6.unspecified = 0 &&
      Ipaddr.V6.Prefix.mem
        (Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.shift buf 24))
        solicited_node_prefix &&
      Ipv6_wire.get_ipv6_hlim buf = 255 &&
      Ipv6_wire.get_ipv6_nhdr buf = Ipv6_wire.protocol_to_int `ICMP &&
      Ipv6_wire.get_ns_ty icmpbuf = 135 &&
      Ipv6_wire.get_ns_code icmpbuf = 0
    then
      Some (Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.shift icmpbuf 8))
    else
      None
  in
  let na addr buf =
    Ipv6_wire.set_ipv6_version_flow buf 0x60000000l; (* IPv6 *)
    Ipv6_wire.set_ipv6_len buf (Ipv6_wire.sizeof_na + Ipv6_wire.sizeof_llopt);
    Ipaddr_cstruct.V6.write_cstruct_exn addr (Cstruct.shift buf 8);
    Ipaddr_cstruct.V6.write_cstruct_exn Ipaddr.V6.link_nodes (Cstruct.shift buf 24);
    Ipv6_wire.set_ipv6_hlim buf 255;
    Ipv6_wire.set_ipv6_nhdr buf (Ipv6_wire.protocol_to_int `ICMP);
    let hdr, icmpbuf = Cstruct.split buf Ipv6_wire.sizeof_ipv6 in
    Ipv6_wire.set_na_ty icmpbuf 136; (* NA *)
    Ipv6_wire.set_na_code icmpbuf 0;
    Ipv6_wire.set_na_reserved icmpbuf 0x20000000l;
    Ipaddr_cstruct.V6.write_cstruct_exn addr (Cstruct.shift icmpbuf 8);
    let optbuf = Cstruct.shift icmpbuf Ipv6_wire.sizeof_na in
    Ipv6_wire.set_llopt_ty optbuf 2;
    Ipv6_wire.set_llopt_len optbuf 1;
    Macaddr_cstruct.write_cstruct_exn mac (Cstruct.shift optbuf 2);
    Ipv6_wire.set_icmpv6_csum icmpbuf 0;
    Ipv6_wire.set_icmpv6_csum icmpbuf @@ Ndpv6.checksum hdr [icmpbuf];
    na_size
  in
  Eio.Fiber.any [
    (fun () -> listen_raw (fun buf ->
         match is_ns buf with
         | None -> ()
         | Some addr ->
           let dst = multicast_mac Ipaddr.V6.link_nodes in
           let buf = Cstruct.create na_size in
           let _ = na addr buf in
           write_raw dst [buf] |> ignore));
    (fun () -> 
      try 
        get_stack ~sw ~clock backend address |> ignore; 
        Alcotest.fail "Expected stack initialization failure"
      with _ -> ());
    (fun () -> 
      Eio.Time.sleep clock 5.;
      Eio.Fiber.check ();
      Alcotest.fail "stack initialization should have failed")
  ]

let suite = [
  "Send a UDP packet from one IPV6 stack and check it is received by another", `Quick, run pass_udp_traffic;
  "NA is sent when a ND is received", `Quick,  run dad_na_is_sent;
  "NA is received, stack fails to initialise", `Quick,  run dad_na_is_received;
]
