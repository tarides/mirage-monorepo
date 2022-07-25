let server_cidr = Ipaddr.V4.Prefix.of_string_exn "192.168.1.254/24"
let client_cidr = Ipaddr.V4.Prefix.of_string_exn "192.168.1.10/24"

let server_port = 7

module Backend = Vnetif_backends.Frame_size_enforced
module Stack = Vnetif_common.VNETIF_STACK(Backend)

let default_mtu = 1500

let err_fail e =
  let err = Format.asprintf "%s" (Printexc.to_string e) in
  Alcotest.fail err

let buffer = Cstruct.create 10000

let rec read_all flow so_far =
  match Eio.Flow.read flow buffer with
  | s -> read_all flow (Cstruct.of_string (Cstruct.copy buffer 0 s) :: so_far)
  | exception End_of_file -> List.rev so_far

let read_one flow =
  match Eio.Flow.read flow buffer with
  | s -> Cstruct.sub buffer 0 s
  | exception End_of_file -> Alcotest.fail "received EOF when we expected at least some data from read"

let get_stacks ~sw ~clock ?client_mtu ?server_mtu backend =
  let or_default = function | None -> default_mtu | Some n -> n in
  let client_mtu, server_mtu = or_default client_mtu, or_default server_mtu in
  let client = Stack.create_stack ~sw ~clock ~cidr:client_cidr ~mtu:client_mtu backend in
  let server = Stack.create_stack ~sw ~clock ~cidr:server_cidr ~mtu:server_mtu backend in
  let max_mtu = max client_mtu server_mtu in
  Backend.set_max_ip_mtu backend max_mtu;
  (server, client)

let start_server ~f server =
  Stack.Stackv4.TCPV4.listen (Stack.Stackv4.tcpv4 server) ~port:server_port f;
  Stack.Stackv4.listen server

let start_client client =
  Stack.Stackv4.TCPV4.create_connection (Stack.Stackv4.tcpv4 client) (Ipaddr.V4.Prefix.address server_cidr, server_port)

let connect ~sw ~env () =
  let clock = env#clock in
  let backend = Backend.create ~sw ~clock () in
  let (server, client) = get_stacks ~sw ~clock ~server_mtu:9000 backend in
  Eio.Fiber.fork ~sw (fun () -> start_server ~f:(fun _ -> ()) server);
  let flow = start_client client in
  Eio.Flow.close flow

let big_server_response ~sw ~env () =
  let clock = env#clock in
  let response = Cstruct.create 7000 in
  Cstruct.memset response 255;
  let backend = Backend.create ~sw ~clock () in
  let (server, client) = get_stacks  ~sw ~clock ~client_mtu:1500 ~server_mtu:9000 backend in
  let f flow =
    Eio.Flow.(copy (cstruct_source [response])) flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork ~sw (fun () -> start_server ~f server);
  let flow = start_client client in
  let l = read_all flow [] in
  Alcotest.(check int) "received size matches sent size" (Cstruct.length response) (Cstruct.length (Cstruct.concat l));
  Eio.Flow.close flow

let big_client_request_chunked ~sw ~env () =
  let clock = env#clock in
  let request = Cstruct.create 3750 in
  Cstruct.memset request 255;
  let backend = Backend.create ~sw ~clock () in
  let (server, client) = get_stacks ~sw ~clock ~client_mtu:1500 ~server_mtu:9000 backend in
  let f flow =
    Eio.Flow.(copy (cstruct_source [request])) flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork ~sw (fun () -> start_server ~f:(fun _flow -> ()) server);
  f (start_client client)

let big_server_response_not_chunked ~sw ~env () =
  let clock = env#clock in
  let response = Cstruct.create 7000 in
  Cstruct.memset response 255;
  let backend = Backend.create ~sw ~clock () in
  let (server, client) = get_stacks ~sw ~clock ~client_mtu:9000 ~server_mtu:9000 backend in
  let f flow =
    Eio.Flow.(copy (cstruct_source [response])) flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork ~sw (fun () -> start_server ~f server);
  let flow = start_client client in 
  let buf = read_one flow in
  Alcotest.(check int) "received size matches sent size" (Cstruct.length response) (Cstruct.length buf);
  Eio.Flow.close flow

let long_comms amt timeout ~sw ~env () =
  (* use the iperf tests to test long-running communication between
   * the two stacks with their different link settings.
   * this helps us find bugs in situations like the TCP window expanding
   * to be larger than the MTU, and the implementation failing to 
   * limit the size of the sent packet in that case. *)
  let module Test = Test_iperf.Test_iperf(Backend) in
  let clock = env#clock in
  let dir = Eio.Stdenv.fs env in
  let backend = Backend.create ~sw ~clock () in
  let (server, client) = get_stacks ~sw ~clock ~client_mtu:1500 ~server_mtu:9000 backend in
  Test.V.record_pcap ~dir backend
    (Printf.sprintf "tcp_mtus_long_comms_%d.pcap" amt)
    (Test.tcp_iperf ~clock ~server ~client amt timeout)

open Common

let suite = [
  "connections work", `Quick, run connect;
  "large server responses are received", `Quick, run big_server_response;
  "large client requests are chunked properly", `Quick, run big_client_request_chunked;
  "large messages aren't unnecessarily segmented", `Quick, run big_server_response_not_chunked;
  "iperf test doesn't crash", `Quick, run (long_comms Test_iperf.amt_quick 120.0);
]
 