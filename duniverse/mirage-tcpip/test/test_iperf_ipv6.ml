(*
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
 * Copyright (c) 2012 Balraj Singh <balraj.singh@cl.cam.ac.uk>
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

module Test_iperf_ipv6 (B : Vnetif_backends.Backend) = struct

  module V = VNETIF_STACK (B)

  let client_ip = Ipaddr.V6.of_string_exn "fc00::23"
  let client_cidr = Ipaddr.V6.Prefix.make 64 client_ip
  let server_ip =  Ipaddr.V6.of_string_exn "fc00::45"
  let server_cidr =  Ipaddr.V6.Prefix.make 64 server_ip

  type stats = {
    mutable bytes: int64;
    mutable packets: int64;
    mutable bin_bytes:int64;
    mutable bin_packets: int64;
    mutable start_time: int64;
    mutable last_time: int64;
  }

  type network = {
    backend : B.t;
    server : V.Stackv6.t;
    client : V.Stackv6.t;
  }

  let default_network ~sw ~clock ?mtu ?(backend = B.create ~sw ~clock ()) () =
    let client = V.create_stack_v6 ~sw ~clock ?mtu ~cidr:client_cidr backend in
    let server = V.create_stack_v6 ~sw ~clock ?mtu ~cidr:server_cidr backend in
    {backend; server; client}

  let msg =
    let m = "01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    let rec build l = function
            | 0 -> l
            | n -> build (m :: l) (n - 1)
    in
    String.concat "" @@ build [] 60

  let mlen = String.length msg

  let err_eof () = failf "EOF while writing to TCP flow"

  let err_connect e ip port () =
    let err = Format.asprintf "%s" (Printexc.to_string e) in
    let ip  = Ipaddr.V6.to_string ip in
    failf "Unable to connect to %s:%d: %s" ip port err

  let err_write e () =
    let err = Format.asprintf "%s" (Printexc.to_string e) in
    failf "Error while writing to TCP flow: %s" err

  let err_read e () =
    let err = Format.asprintf "%s" (Printexc.to_string e) in
    failf "Error in server while reading: %s" err

  let write_and_check flow buf =
    match Eio.Flow.(copy (cstruct_source [buf])) flow with
    | ()          -> ()
    | exception End_of_file ->
      Eio.Flow.close flow; 
      err_eof ()
    | exception exn -> 
      Eio.Flow.close flow; 
      raise exn
  
  let tcp_connect t (ip, port) =
    match V.Stackv6.TCP.create_connection t (ip, port) with
    | exception e -> err_connect e ip port ()
    | f    -> f

  let iperfclient s amt dest_ip dport =
    let iperftx flow =
      Logs.info (fun f -> f  "Iperf client: Made connection to server.");
      let a = Cstruct.create mlen in
      Cstruct.blit_from_string msg 0 a 0 mlen;
      let rec loop = function
        | 0 -> ()
        | n -> (write_and_check flow a; loop (n-1))
      in
      loop (amt / mlen);
      let a = Cstruct.sub a 0 (amt - (mlen * (amt/mlen))) in
      write_and_check flow a;
      Eio.Flow.close flow
    in
    Logs.info (fun f -> f  "Iperf client: Attempting connection.");
    let flow = tcp_connect (V.Stackv6.tcp s) (dest_ip, dport) in
    iperftx flow;
    Logs.debug (fun f -> f  "Iperf client: Done.")

  let print_data st ts_now =
    let server = Int64.sub ts_now st.start_time in
    let rate_in_mbps =
        let t_in_s = Int64.(to_float (sub ts_now st.last_time)) /. 1_000_000_000. in
        (Int64.to_float st.bin_bytes) /. t_in_s /. 125000.
    in
    let live_words = Gc.((stat()).live_words) in
    Logs.info (fun f -> f  "Iperf server: t = %.0Lu, avg_rate = %0.2f MBits/s, totbytes = %Ld, \
                             live_words = %d" server rate_in_mbps st.bytes live_words);
    st.last_time <- ts_now;
    st.bin_bytes <- 0L;
    st.bin_packets <- 0L

  let iperf _s server_done_u flow =
    (* debug is too much for us here *)
    Logs.set_level ~all:true (Some Logs.Info);
    Logs.info (fun f -> f  "Iperf server: Received connection.");
    let t0 = Clock.elapsed_ns () in
    let st = {
      bytes=0L; packets=0L; bin_bytes=0L; bin_packets=0L; start_time = t0;
      last_time = t0
    } in
    let rec iperf_h flow =
      let buffer = Cstruct.create 2048 in
      match Eio.Flow.read flow buffer with
      | exception End_of_file ->
        let ts_now = Clock.elapsed_ns () in
        st.bin_bytes <- st.bytes;
        st.bin_packets <- st.packets;
        st.last_time <- st.start_time;
        print_data st ts_now;
        Eio.Flow.close flow;
        Logs.info (fun f -> f  "Iperf server: Done - closed connection.")
      | l ->
        begin
          let _data = Cstruct.sub buffer 0 l in
          st.bytes <- (Int64.add st.bytes (Int64.of_int l));
          st.packets <- (Int64.add st.packets 1L);
          st.bin_bytes <- (Int64.add st.bin_bytes (Int64.of_int l));
          st.bin_packets <- (Int64.add st.bin_packets 1L);
          let ts_now = Clock.elapsed_ns () in
          (if (Int64.sub ts_now st.last_time >= 1_000_000_000L) then
             print_data st ts_now);
          iperf_h flow
        end
    in
    iperf_h flow;
    Eio.Promise.resolve server_done_u ()

  let tcp_iperf ~sw ~clock ~server ~client amt timeout () =
    let port = 5001 in

    let server_ready, server_ready_u = Eio.Promise.create () in
    let server_done, server_done_u = Eio.Promise.create () in
    let server_s, client_s = server, client in

    let ip_of s = V.Stackv6.IP.get_ip (V.Stackv6.ip s) |> List.hd in

    Eio.Fiber.any [
      (fun () -> 
        Eio.Time.sleep clock timeout; (* timeout *)
        failf "iperf test timed out after %f seconds" timeout);
      
      (fun () -> 
        Eio.Promise.await server_ready;
        Eio.Time.sleep clock 0.1; (* Give server 0.1 s to call listen *)
        Logs.info (fun f -> f  "I am client with IP %a, trying to connect to server @ %a:%d"
          Ipaddr.V6.pp (ip_of client_s) Ipaddr.V6.pp (ip_of server_s) port);
        Eio.Fiber.fork ~sw (fun () -> V.Stackv6.listen client_s);
        iperfclient client_s amt (ip_of server) port);

      (fun () -> 
        Logs.info (fun f -> f  "I am server with IP %a, expecting connections on port %d"
          Ipaddr.V6.pp (V.Stackv6.IP.get_ip (V.Stackv6.ip server_s) |> List.hd)
          port);
        V.Stackv6.TCP.listen (V.Stackv6.tcp server_s) ~port (iperf server_s server_done_u);
        Eio.Promise.resolve server_ready_u ();
        V.Stackv6.listen server_s) ];

    Logs.info (fun f -> f  "Waiting for server_done...");
    Eio.Promise.await server_done (* exit cleanly *)
end

let test_tcp_iperf_ipv6_two_stacks_basic ~sw ~env amt timeout () =
  let clock, dir = env#clock, env#fs in
  let module Test = Test_iperf_ipv6 (Vnetif_backends.Basic) in
  let { backend; Test.client; Test.server } = Test.default_network ~sw ~clock () in
  Test.V.record_pcap ~dir backend
    (Printf.sprintf "tcp_iperf_ipv6_two_stacks_basic_%d.pcap" amt)
    (Test.tcp_iperf  ~sw ~clock ~server ~client amt timeout)

let test_tcp_iperf_ipv6_two_stacks_mtu  ~sw ~env amt timeout () =
  let clock, dir = env#clock, env#fs in
  let mtu = 1500 in
  let module Test = Test_iperf_ipv6 (Vnetif_backends.Frame_size_enforced) in
  let backend = Vnetif_backends.Frame_size_enforced.create ~sw ~clock () in
  Vnetif_backends.Frame_size_enforced.set_max_ip_mtu backend mtu;
  let { backend; Test.client; Test.server } = Test.default_network ~sw ~clock ?mtu:(Some mtu) ?backend:(Some backend) () in
  Test.V.record_pcap ~dir backend
    (Printf.sprintf "tcp_iperf_ipv6_two_stacks_mtu_%d.pcap" amt)
    (Test.tcp_iperf ~sw ~clock ~server ~client amt timeout)

let test_tcp_iperf_ipv6_two_stacks_trailing_bytes ~sw ~env amt timeout () =
  let clock, dir = env#clock, env#fs in
  let module Test = Test_iperf_ipv6 (Vnetif_backends.Trailing_bytes) in
  let { backend; Test.client; Test.server } = Test.default_network ~sw ~clock () in
  Test.V.record_pcap ~dir backend
    (Printf.sprintf "tcp_iperf_ipv6_two_stacks_trailing_bytes_%d.pcap" amt)
    (Test.tcp_iperf ~sw ~clock ~server ~client amt timeout)

let test_tcp_iperf_ipv6_two_stacks_uniform_packet_loss ~sw ~env amt timeout () =
  let clock, dir = env#clock, env#fs in
  let module Test = Test_iperf_ipv6 (Vnetif_backends.Uniform_packet_loss) in
  let { backend; Test.client; Test.server } = Test.default_network ~sw ~clock () in
  Test.V.record_pcap ~dir backend
    (Printf.sprintf "tcp_iperf_ipv6_two_stacks_uniform_packet_loss_%d.pcap" amt)
    (Test.tcp_iperf ~sw ~clock ~server ~client amt timeout)

let test_tcp_iperf_ipv6_two_stacks_uniform_packet_loss_no_payload ~sw ~env amt timeout () =
  let clock, dir = env#clock, env#fs in
  let module Test = Test_iperf_ipv6 (Vnetif_backends.Uniform_no_payload_packet_loss) in
  let { backend; Test.client; Test.server } = Test.default_network ~sw ~clock () in
  Test.V.record_pcap ~dir backend
    (Printf.sprintf "tcp_iperf_ipv6_two_stacks_uniform_packet_loss_no_payload_%d.pcap" amt)
    (Test.tcp_iperf ~sw ~clock ~server ~client amt timeout)

let test_tcp_iperf_ipv6_two_stacks_drop_1sec_after_1mb ~sw ~env amt timeout () =
  let clock, dir = env#clock, env#fs in
  let module Test = Test_iperf_ipv6 (Vnetif_backends.Drop_1_second_after_1_megabyte) in
  let { backend; Test.client; Test.server } = Test.default_network ~sw ~clock () in
  Test.V.record_pcap ~dir backend
    "tcp_iperf_ipv6_two_stacks_drop_1sec_after_1mb.pcap"
    (Test.tcp_iperf ~sw ~clock ~server ~client amt timeout)

let amt_quick = 100_000
let amt_slow  = amt_quick * 1000

open Common

let suite = [

  "iperf with two stacks, basic tests", `Quick,
  run @@ test_tcp_iperf_ipv6_two_stacks_basic amt_quick 120.0;

  "iperf with two stacks, over an MTU-enforcing backend", `Quick,
  run @@ test_tcp_iperf_ipv6_two_stacks_mtu amt_quick 120.0;

  "iperf with two stacks, testing trailing_bytes", `Quick,
  run @@ test_tcp_iperf_ipv6_two_stacks_trailing_bytes amt_quick 120.0;

  "iperf with two stacks and uniform packet loss", `Quick,
  run @@ test_tcp_iperf_ipv6_two_stacks_uniform_packet_loss amt_quick 120.0;

  "iperf with two stacks and uniform packet loss of packets with no payload", `Slow,
  run @@ test_tcp_iperf_ipv6_two_stacks_uniform_packet_loss_no_payload amt_quick 240.0;

  "iperf with two stacks and uniform packet loss of packets with no payload, longer", `Slow,
  run @@ test_tcp_iperf_ipv6_two_stacks_uniform_packet_loss_no_payload amt_slow 240.0;

  "iperf with two stacks, basic tests, longer", `Slow,
  run @@ test_tcp_iperf_ipv6_two_stacks_basic amt_slow 240.0;

  "iperf with two stacks and uniform packet loss, longer", `Slow,
  run @@ test_tcp_iperf_ipv6_two_stacks_uniform_packet_loss amt_slow 240.0;

  "iperf with two stacks drop 1 sec after 1 mb", `Quick,
  run @@ test_tcp_iperf_ipv6_two_stacks_drop_1sec_after_1mb amt_quick 120.0;

]
