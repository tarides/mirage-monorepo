(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
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

open Common
open Vnetif_common

let src = Logs.Src.create "test_connect" ~doc:"connect tests"
module Log = (val Logs.src_log src : Logs.LOG)

module Test_connect_ipv6 (B : Vnetif_backends.Backend) = struct
  module V = VNETIF_STACK (B)

  let client_address = Ipaddr.V6.of_string_exn "fc00::23"
  let client_cidr = Ipaddr.V6.Prefix.make 64 client_address
  let server_address = Ipaddr.V6.of_string_exn "fc00::45"
  let server_cidr = Ipaddr.V6.Prefix.make 64 server_address
  let test_string = "Hello world from Mirage 123456789...."
  let backend = V.create_backend ()

  let err_read_eof () = failf "accept got EOF while reading"
  let err_write_eof () = failf "client tried to write, got EOF"

  let buffer = Cstruct.create 10000

  let accept ~clock flow expected =
    let ip, port = flow#dst in
    Log.debug (fun f -> f "Accepted connection from %s:%d" (Ipaddr.V6.to_string ip) port);
    match Eio.Flow.read flow buffer with
    | exception End_of_file      -> err_read_eof ()
    | sz ->
      let b = Cstruct.sub buffer 0 sz in
      Eio.Time.sleep clock 0.1;
      (* sleep first to capture data in pcap *)
      Alcotest.(check string) "accept" expected (Cstruct.to_string b);
      Log.debug (fun f -> f "Connection closed")

  let test_tcp_connect_two_stacks ~sw:_ ~clock backend () =
    let timeout = 15.0 in
    let fast_clock = fast_clock clock in
    Common.switch_run_cancel_on_return @@ fun sw ->
    Eio.Fiber.any ~label:"test_connect_ipv6" [
      (fun () -> 
        Eio.Time.sleep clock timeout;
        failf "connect test timedout after %f seconds" timeout) ;

      (fun () -> 
        let s1 = V.create_stack_v6 ~sw ~clock:fast_clock ~cidr:server_cidr backend in
        V.Stackv6.TCP.listen (V.Stackv6.tcp s1) ~port:80 (fun f -> accept ~clock:fast_clock f test_string);
        V.Stackv6.listen s1) ;

      (fun () ->
        Eio.Time.sleep clock 0.1;
        let s2 = V.create_stack_v6 ~sw ~clock:fast_clock ~cidr:client_cidr backend in
        Eio.Fiber.any [
          (fun () -> V.Stackv6.listen s2);
          (fun () -> 
            let conn = V.Stackv6.TCP.create_connection (V.Stackv6.tcp s2) in
            let flow = conn (server_address, 80) in
            Log.debug (fun f -> f "Connected to other end...");
            match Eio.Flow.copy_string test_string flow with
            | exception End_of_file -> err_write_eof ()
            | ()   ->
              Log.err (fun f -> f "wrote hello world");
              Eio.Flow.close flow;
              Eio.Time.sleep fast_clock 1.0 (* record some traffic after close *)
              )]) ]

  let record_pcap = V.record_pcap

end

let test_tcp_connect_two_stacks_basic ~sw ~env () =
  let clock, dir = env#clock, env#fs in
  let module Test = Test_connect_ipv6(Vnetif_backends.Basic) in
  let backend = Test.backend ~sw ~clock in
  Test.record_pcap ~dir backend
    "tcp_connect_ipv6_two_stacks_basic.pcap"
    (Test.test_tcp_connect_two_stacks  ~sw ~clock backend)

let test_tcp_connect_two_stacks_x100_uniform_no_payload_packet_loss ~sw ~env () =
  let clock, dir = env#clock, env#fs in
  let rec loop = function
      | 0 -> ()
      | n -> Log.info (fun f -> f "%d/100" (101-n));
             let module Test = Test_connect_ipv6(Vnetif_backends.Uniform_no_payload_packet_loss) in
             let backend = Test.backend ~sw ~clock in
             Test.record_pcap ~dir backend
               (Printf.sprintf
               "tcp_connect_ipv6_two_stacks_no_payload_packet_loss_%d_of_100.pcap" n)
               (Test.test_tcp_connect_two_stacks ~sw ~clock backend);
             loop (n - 1)
  in
  loop 100

let test_tcp_connect_two_stacks_trailing_bytes ~sw ~env () =
  let clock, dir = env#clock, env#fs in
  let module Test = Test_connect_ipv6(Vnetif_backends.Trailing_bytes) in
  let backend = Test.backend ~sw ~clock in
  Test.record_pcap ~dir backend
    "tcp_connect_ipv6_two_stacks_trailing_bytes.pcap"
    (Test.test_tcp_connect_two_stacks ~sw ~clock backend)

let suite = [

  "connect two stacks, basic test", `Quick,
  run test_tcp_connect_two_stacks_basic;

  "connect two stacks, uniform packet loss of packets with no payload x 100", `Slow,
  run test_tcp_connect_two_stacks_x100_uniform_no_payload_packet_loss;

  "connect two stacks, with trailing bytes", `Quick,
  run test_tcp_connect_two_stacks_trailing_bytes;

]
