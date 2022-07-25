open Common

module B = Basic_backend.Make
module V = Vnetif.Make(B)
module E = Ethernet.Make(V)
module Static_arp = Static_arp.Make(E)
module Ip = Static_ipv4.Make(Mirage_random_test)(Mclock)(E)(Static_arp)
module Udp = Udp.Make(Ip)(Mirage_random_test)

type stack = {
  backend : B.t;
  netif : V.t;
  ethif : E.t;
  arp : Static_arp.t;
  ip : Ip.t;
  udp : Udp.t;
}

let get_stack ~sw ~clock ?(backend = B.create ~use_async_readers:sw ()) ip =
  let cidr = Ipaddr.V4.Prefix.make 24 ip in
  let netif = V.connect backend in
  let ethif = E.connect netif in
  let arp = Static_arp.connect ~sw ~clock ethif in
  let ip = Ip.connect ~cidr ethif arp in
  let udp = Udp.connect ip in
  { backend; netif; ethif; arp; ip; udp }

let fails msg f args =
  match f args with
  | Ok _ -> Alcotest.fail msg
  | Error _ -> ()

let marshal_unmarshal () =
  let parse = Udp_packet.Unmarshal.of_cstruct in
  fails "unmarshal a 0-length packet" parse (Cstruct.create 0);
  fails "unmarshal a too-short packet" parse (Cstruct.create 2);
  let with_data = Cstruct.create 8 in
  Cstruct.memset with_data 0;
  Udp_wire.set_udp_source_port with_data 2000;
  Udp_wire.set_udp_dest_port with_data 21;
  Udp_wire.set_udp_length with_data 20;
  let payload = Cstruct.of_string "abcdefgh1234" in
  let with_data = Cstruct.concat [with_data; payload] in
  match Udp_packet.Unmarshal.of_cstruct with_data with
  | Error s -> Alcotest.fail s
  | Ok (_header, data) ->
    Alcotest.(check cstruct) "unmarshalling gives expected data" payload data

let write ~sw ~env () =
  let dst = Ipaddr.V4.of_string_exn "192.168.4.20" in
  let stack = get_stack ~sw ~clock:env#clock dst in
  Static_arp.add_entry stack.arp dst (Macaddr.of_string_exn "00:16:3e:ab:cd:ef");
  Udp.write ~src_port:1212 ~dst_port:21 ~dst stack.udp (Cstruct.of_string "MGET *")

let unmarshal_regression () =
  let i = Cstruct.create 1016 in
  Cstruct.memset i 30;
  Cstruct.set_char i 4 '\x04';
  Cstruct.set_char i 5 '\x00';
  Alcotest.(check (result reject pass)) "correctly return error for bad packet"
    (Error "parse failed") (Udp_packet.Unmarshal.of_cstruct i)


let marshal_marshal () =
  let error_str = Alcotest.result Alcotest.reject Alcotest.string in
  let udp = {Udp_packet.src_port = 1; dst_port = 2} in
  let payload = Cstruct.create 100 in
  let buffer = Cstruct.create Udp_wire.sizeof_udp in
  let src = Ipaddr.V4.of_string_exn "127.0.0.1" in
  let dst = Ipaddr.V4.of_string_exn "127.0.0.1" in
  let pseudoheader = Ipv4_packet.Marshal.pseudoheader ~src ~dst ~proto:`UDP (Cstruct.length buffer + Cstruct.length payload) in
  Udp_packet.Marshal.into_cstruct ~pseudoheader ~payload udp (Cstruct.shift buffer 1)
  |> Alcotest.check error_str "Buffer too short" (Error "Not enough space for a UDP header");
  Udp_packet.Marshal.into_cstruct ~pseudoheader ~payload udp buffer
  |> Alcotest.(check (result unit string)) "Buffer big enough for header" (Ok ());
  Udp_packet.Unmarshal.of_cstruct (Cstruct.concat [buffer; payload])
  |> Alcotest.(check (result (pair udp_packet cstruct) string)) "Save and reload" (Ok (udp, payload))

let suite = [
  "unmarshal regression", `Quick, unmarshal_regression;
  "marshal/marshal", `Quick, marshal_marshal;
  "marshal/unmarshal", `Quick, marshal_unmarshal;
  "write packets", `Quick, run write;
]
