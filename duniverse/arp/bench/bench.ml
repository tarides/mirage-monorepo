(* derived from ISC-licensed mirage-tcpip/lib_test/test_arp.ml *)

let count2 = ref 0

module Test (R : Mirage_random.S) = struct
let hdr buf =
  Cstruct.BE.set_uint16 buf 0 1 ;
  Cstruct.BE.set_uint16 buf 2 0x0800 ;
  Cstruct.set_uint8 buf 4 6 ;
  Cstruct.set_uint8 buf 5 4

let gen_int () =
  let buf = R.generate 1 in
  Cstruct.get_uint8 buf 0

let gen_op buf off =
  let op = gen_int () in
  let op = 1 + op mod 2 in
  Cstruct.BE.set_uint16 buf off op

let gen_arp buf =
  hdr buf ;
  gen_op buf 6 ;
  let addresses = R.generate 20 in
  Cstruct.blit addresses 0 buf 8 20 ;
  28

let gen_req buf =
  hdr buf ;
  Cstruct.BE.set_uint16 buf 6 1 ;
  let addresses = R.generate 20 in
  Cstruct.blit addresses 0 buf 8 20 ;
  28

let gen_ip () =
  let last = R.generate 1 in
  let ip = "\010\000\000" ^ (Cstruct.to_string last) in
  Ipaddr.V4.of_octets_exn ip

let ip = Ipaddr.V4.of_string_exn "10.0.0.0"
let mac = Macaddr.of_string_exn "00:de:ad:be:ef:00"

let gen_rep buf =
  hdr buf ;
  Cstruct.BE.set_uint16 buf 6 2 ;
  let omac = R.generate 6 in
  Cstruct.blit omac 0 buf 8 6 ;
  let oip = gen_ip () in
  Cstruct.blit_from_string (Ipaddr.V4.to_octets oip) 0 buf 14 4 ;
  Cstruct.blit_from_string (Macaddr.to_octets mac) 0 buf 18 6 ;
  Cstruct.blit_from_string (Ipaddr.V4.to_octets ip) 0 buf 24 4 ;
  28

let other_ip = Ipaddr.V4.of_string_exn "10.0.0.1"
let other_mac = Macaddr.of_string_exn "00:de:ad:be:ef:01"

let myreq buf =
  hdr buf ;
  Cstruct.BE.set_uint16 buf 6 1 ;
  Cstruct.blit_from_string (Macaddr.to_octets other_mac) 0 buf 8 6 ;
  Cstruct.blit_from_string (Ipaddr.V4.to_octets other_ip) 0 buf 14 4 ;
  Cstruct.blit_from_string (Macaddr.to_octets mac) 0 buf 18 6 ;
  Cstruct.blit_from_string (Ipaddr.V4.to_octets ip) 0 buf 24 4 ;
  28

module B = Basic_backend.Make
module V = Vnetif.Make(B)
module E = Ethernet.Make(V)
module A = Arp.Make(E)

let c = ref 0
let gen ~sw arp buf =
  c := !c mod 100 ;
  match !c with
  | x when x >= 00 && x < 10 ->
    let len = gen_int () mod 28 in
    let r = R.generate len in
    Cstruct.blit r 0 buf 0 len ;
    len
  | x when x >= 10 && x < 20 -> gen_req buf
  | x when x >= 20 && x < 50 -> myreq buf
  | x when x >= 50 && x < 80 ->
    if x mod 2 = 0 then
      (let rand = gen_int () in
       for _i = 0 to rand do
         let ip = gen_ip () in
         Eio.Fiber.fork ~sw (fun () -> A.query arp ip |> ignore)
       done) ;
    gen_rep buf
  | x when x >= 80 && x < 100 -> gen_arp buf
  | _ -> invalid_arg "bla"

let rec query ~sw ~clock arp () =
  incr count2 ;
  let ip = gen_ip () in
  Eio.Fiber.fork ~sw (fun () -> A.query arp ip |> ignore);
  Eio.Time.sleep clock (Duration.(of_us 100 |> to_f));
  query ~sw ~clock arp ()

type arp_stack = {
  backend : B.t;
  netif: V.t;
  ethif: E.t;
  arp: A.t;
}

let get_arp ~sw ~clock ?(backend = B.create ~use_async_readers:sw ()) () =
  let netif = V.connect backend in
  let ethif = E.connect netif in
  let arp = A.connect ~sw ~clock ethif  in
  { backend; netif; ethif; arp }

let rec send ~clock ethernet gen () =
  let buffer = Cstruct.create Arp_packet.size in
  gen buffer |> ignore;
  E.writev ethernet Macaddr.broadcast `ARP [buffer];
  Eio.Fiber.yield ();
  send ~clock ethernet gen ()

let header_size = Ethernet.Packet.sizeof_ethernet


let runit ~sw ~clock () =
  let send = send ~clock in
  let sleep () = 
    Eio.Time.sleep clock 5. 
  in
  Printf.printf "starting\n%!";
  let stack = get_arp ~sw ~clock () in
  let other = get_arp ~sw ~clock ~backend:stack.backend () in
  A.set_ips stack.arp [ip];
  let count = ref 0 in
  Printf.printf "ready\n%!";
  Eio.Fiber.any [
    (fun () -> V.listen stack.netif ~header_size (fun b -> incr count ; A.input stack.arp b) |> ignore);
    send other.ethif (fun b ->
      let res = R.generate 28 in
      Cstruct.blit res 0 b 0 28 ;
      28);
    sleep
  ];
  Printf.printf "%d random input\n%!" !count ;
  count := 0 ;
  Eio.Fiber.any [
    (fun () -> V.listen stack.netif ~header_size (fun b -> incr count ; A.input stack.arp b) |> ignore);
    send other.ethif gen_arp ;
    sleep
  ];
  Printf.printf "%d random ARP input\n%!" !count ;
  count := 0 ;
  Eio.Fiber.any [
    (fun () -> V.listen stack.netif ~header_size (fun b -> incr count ; A.input stack.arp b) |> ignore);
    send other.ethif gen_req ;
    sleep
  ];
  Printf.printf "%d requests\n%!" !count ;
  count := 0 ;
  Eio.Fiber.any [
    (fun () -> V.listen stack.netif ~header_size (fun b -> incr count ; A.input stack.arp b) |> ignore);
    send other.ethif gen_rep ;
    sleep
  ];
  Printf.printf "%d replies\n%!" !count ;
  count := 0 ;
  Eio.Fiber.any [
    (fun () -> V.listen stack.netif ~header_size (fun b -> incr count ; A.input stack.arp b) |> ignore);
    send other.ethif (gen ~sw stack.arp) ;
    sleep
  ];
  Printf.printf "%d mixed\n%!" !count ;
  count := 0 ;
  Eio.Fiber.any [
    (fun () -> V.listen stack.netif ~header_size (fun b -> incr count ; A.input stack.arp b) |> ignore);
    send other.ethif gen_rep;
    query ~sw ~clock stack.arp;
    sleep
  ] |> ignore;
  Printf.printf "%d queries (%d qs)\n%!" !count !count2
end

let run_dir program () =
  Eio_linux.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let dir = Eio.Stdenv.fs env in
  try
    Eio.Switch.run @@ fun sw ->
    program ~sw ~clock ~dir ();
    Eio.Switch.fail sw Not_found
  with Not_found -> ()

let run program =
  run_dir (fun ~sw ~clock ~dir:_ () -> program ~sw ~clock ())

module T = Test(Mirage_random_test)
let () =
  Mirage_random_test.initialize () ;
  run T.runit ();
  count2 := 0 ;
  run T.runit ();
  count2 := 0 ;
  run T.runit ()
