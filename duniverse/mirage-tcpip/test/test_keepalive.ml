(* Test the functional part *)

(* Linux default *)
let default = Tcpip.Tcp.Keepalive.({
  after = Duration.of_sec 7200; (* 2 hours *)
  interval = Duration.of_sec 75; (* 75 seconds *)
  probes = 9;
})

let simulate configuration iterations nprobes ns state =
  let rec loop iterations nprobes ns state =
    if iterations > 3 * configuration.Tcpip.Tcp.Keepalive.probes
    then Alcotest.fail (Printf.sprintf "too many iteractions: loop in keep-alive test? iterations = %d nprobes = %d ns=%Ld" iterations nprobes ns);
    let action, state' = Tcp.Keepalive.next ~configuration ~ns state in
    match action with
    | `SendProbe ->
      Logs.info (fun f -> f "iteration %d, ns %Ld: SendProbe" iterations ns);
      loop (iterations + 1) (nprobes + 1) ns state'
    | `Wait ns' ->
      Logs.info (fun f -> f "iteration %d, ns %Ld: Wait %Ld" iterations ns ns');
      loop (iterations + 1) nprobes (Int64.add ns ns') state'
    | `Close ->
      Logs.info (fun f -> f "iteration %d, ns %Ld: Close" iterations ns);
      nprobes in
  loop iterations nprobes ns state

(* check we send the expected number of probes if everything does as expected *)
let test_keepalive_sequence () =
  let configuration = default in
  let state = Tcp.Keepalive.alive in
  let nprobes = simulate configuration 0 0 0L state in
  Alcotest.(check int) "number of probes" (configuration.probes) nprobes

(* check what happens if we miss a probe *)
let test_keepalive_miss_probes () =
  let configuration = default in
  let state = Tcp.Keepalive.alive in
  (* skip sending the first 1 or 2 probes *)
  let ns = Int64.(add configuration.Tcpip.Tcp.Keepalive.after (mul 2L configuration.Tcpip.Tcp.Keepalive.interval)) in
  let nprobes = simulate configuration 0 0 ns state in
  if nprobes >= configuration.Tcpip.Tcp.Keepalive.probes
  then Alcotest.fail (Printf.sprintf "too many probes: max was %d but we sent %d and we should have skipped the first 1 or 2" configuration.probes nprobes)

(* check what happens if we exceed the maximum timeout *)
let test_keepalive_miss_everything () =
  let configuration = default in
  let state = Tcp.Keepalive.alive in
  (* massive delay *)
  let ns = Int64.(add configuration.Tcpip.Tcp.Keepalive.after (mul 2L (mul (of_int configuration.Tcpip.Tcp.Keepalive.probes) configuration.Tcpip.Tcp.Keepalive.interval))) in
  let nprobes = simulate configuration 0 0 ns state in
  if nprobes <> 0
  then Alcotest.fail (Printf.sprintf "too many probes: max was %d but we sent %d and we should have skipped all" configuration.probes nprobes)

let suite_1 = [
  "correct number of keepalives", `Quick, test_keepalive_sequence;
  "we don't try to send old keepalives", `Quick, test_keepalive_miss_probes;
  "check we close if we miss all probes", `Slow, test_keepalive_miss_everything;
]

(* Test the end-to-end protocol behaviour *)
open Common
open Vnetif_common

let src = Logs.Src.create "test_keepalive" ~doc:"keepalive tests"
module Log = (val Logs.src_log src : Logs.LOG)

(* Establish a TCP connection, enable keepalives on the connection, tell the network
   to drop all packets and check that the keep-alives detect the failure. *)
module Test_connect = struct
  module V = VNETIF_STACK (Vnetif_backends.On_off_switch)

  let gateway = Ipaddr.V4.of_string_exn "10.0.0.1"
  let client_cidr = Ipaddr.V4.Prefix.of_string_exn "10.0.0.101/24"
  let server_cidr = Ipaddr.V4.Prefix.of_string_exn "10.0.0.100/24"

  let err_read_eof () = failf "accept got EOF while reading"
  let err_write_eof () = failf "client tried to write, got EOF"

  let err e =
    let err = Format.asprintf "%s" (Printexc.to_string e) in
    failf "Error while reading: %s" err  

  let accept flow =
    let ip, port = flow#dst in
    Logs.debug (fun f -> f "Accepted connection from %s:%d" (Ipaddr.V4.to_string ip) port);
    let buf = Cstruct.create 10000 in
    match Eio.Flow.read flow buf with
    | exception End_of_file -> ()
    | _ -> failf "accept: expected to get EOF in read, but got data"

  let test_tcp_keepalive_timeout ~sw ~clock backend () =
    let timeout = 15.0 in
    Eio.Fiber.any [
      (fun () -> 
        Eio.Time.sleep clock timeout;
        failf "connect test timedout after %f seconds" timeout) ;

      (fun () -> 
        let s1 = V.create_stack ~sw ~clock ~cidr:server_cidr ~gateway backend in
        V.Stackv4.TCPV4.listen (V.Stackv4.tcpv4 s1) ~port:80 (fun f -> accept f);
        V.Stackv4.listen s1) ;

      (fun () -> 
        Eio.Time.sleep clock 0.1;
        let s2 = V.create_stack ~sw ~clock ~cidr:client_cidr ~gateway backend in
        Eio.Fiber.any [
          (fun () -> V.Stackv4.listen s2);
          (fun () ->
            let keepalive = { Tcpip.Tcp.Keepalive.after = 0L; interval = Duration.of_sec 1; probes = 3 } in
            let conn = V.Stackv4.TCPV4.create_connection ~keepalive (V.Stackv4.tcpv4 s2) in
            let flow = conn (Ipaddr.V4.Prefix.address server_cidr, 80) in
            Logs.debug (fun f -> f "Connected to other end...");
            Vnetif_backends.On_off_switch.send_packets := false;
            let buf = Cstruct.create 1000 in
            match Eio.Flow.read flow buf with
            | _ -> failf "read: expected to get EOF, but got data"
            | exception End_of_file ->
              Logs.debug (fun f -> f "connection read EOF as expected");
              Eio.Flow.close flow;
              Eio.Time.sleep clock 1.0 (* record some traffic after close *)
        )]) 
      ]
  let record_pcap = V.record_pcap

end

let test_tcp_keepalive_timeout ~sw ~env () =
  let clock, dir = env#clock, env#fs in
  let backend = Test_connect.V.create_backend ~sw ~clock () in
  Test_connect.record_pcap ~dir backend
    "test_tcp_keepalive_timeout.pcap"
    (Test_connect.test_tcp_keepalive_timeout ~sw ~clock backend)

let suite_2 = [
  "check that TCP keepalives detect a network failure", `Slow,
  run test_tcp_keepalive_timeout;
]

let suite = suite_1 @ suite_2
