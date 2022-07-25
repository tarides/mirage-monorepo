(*
 * Copyright (c) 2016 Pablo Polvorin <pablo.polvorin@gmail.com>
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
module Lwt = struct end

(*
 * Connects two stacks to the same backend.
 * One is a complete v4 stack (the system under test, referred to as [sut]).
 * The other gives us low level access to inject crafted TCP packets,
 * and sends and receives crafted packets to check the [sut] behavior.
 *)
module VNETIF_STACK = Vnetif_common.VNETIF_STACK (Vnetif_backends.Basic)
module V = Vnetif.Make (Vnetif_backends.Basic)
module E = Ethernet.Make (V)
module A = Arp.Make (E)
module I = Static_ipv4.Make (Mirage_random_test) (Vnetif_common.Clock) (E) (A)
module Wire = Tcp.Wire
module WIRE = Wire.Make (I)
module Tcp_wire = Tcp.Tcp_wire
module Tcp_unmarshal = Tcp.Tcp_packet.Unmarshal
module Sequence = Tcp.Sequence

let sut_cidr = Ipaddr.V4.Prefix.of_string_exn "10.0.0.101/24"
let server_ip = Ipaddr.V4.of_string_exn "10.0.0.100"
let server_cidr = Ipaddr.V4.Prefix.make 24 server_ip
let gateway = Ipaddr.V4.of_string_exn "10.0.0.1"
let header_size = Ethernet.Packet.sizeof_ethernet

(* defaults when injecting packets *)
let options = []
let window = 5120

let create_sut_stack backend =
  VNETIF_STACK.create_stack ~cidr:sut_cidr ~gateway backend

let create_raw_stack ~sw ~clock backend =
  let netif = V.connect backend in
  let ethif = E.connect netif in
  let arpv4 = A.connect ~sw ~clock ethif  in
  let ip = I.connect ~cidr:server_cidr ~gateway ethif arpv4 in
  (netif, ethif, arpv4, ip)

type 'state fsm_result = Fsm_next of 'state | Fsm_done | Fsm_error of string

(*  This could be moved to a common module and reused for other low level tcp tests *)

(* setups network and run a given sut and raw fsm *)
let run ~sw ~clock backend fsm sut () =
  let initial_state, fsm_handler = fsm in
  let stackv4 = create_sut_stack ~sw ~clock backend in
  let netif, ethif, arp, rawip = create_raw_stack ~sw ~clock backend in
  let disconnect_raw_stack_promise, disconnect_raw_stack =
    Eio.Promise.create ()
  in
  let error_mbox = Eio.Stream.create 1 in
  let stream = Eio.Stream.create max_int in
  (* Consume TCP packets one by one, in sequence *)
  let rec fsm_thread state =
    let src, dst, data = Eio.Stream.take stream in
    match fsm_handler rawip state ~src ~dst data with
    | Fsm_next s -> fsm_thread s
    | Fsm_done -> ()
    | Fsm_error err ->
        Eio.Stream.add error_mbox err;
        (* it will be terminated anyway when the error is picked up *)
        fsm_thread state
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Fiber.first
        (fun () ->
          V.listen netif ~header_size
            (E.input ~arpv4:(A.input arp)
               ~ipv4:
                 (I.input
                    ~tcp:(fun ~src ~dst data ->
                      Eio.Stream.add stream (src, dst, data))
                    ~udp:(fun ~src:_ ~dst:_ _data -> ())
                    ~default:(fun ~proto ~src ~dst _data ->
                      Logs.debug (fun f ->
                          f
                            "default handler invoked for packet from %a to %a, \
                             protocol %d -- dropping"
                            Ipaddr.V4.pp src Ipaddr.V4.pp dst proto))
                    rawip)
               ~ipv6:(fun _buf ->
                 Logs.debug (fun f -> f "IPv6 packet -- dropping"))
               ethif)
          |> ignore)
        (fun () -> Eio.Promise.await disconnect_raw_stack_promise));
  (* Either both fsm and the sut terminates, or a timeout occurs, or one of the sut/fsm informs an error *)
  Eio.Fiber.any
    [
      (fun () ->
        Eio.Time.sleep clock 5.;
        Some "timed out");
      (fun () ->
        Eio.Fiber.all
          [
            (fun () -> fsm_thread initial_state);
            (fun () ->
              (* time to let the other end connects to the network and listen.
                 * Otherwise initial syn might need to be repeated slowing down the test *)
              Eio.Time.sleep clock 0.1;
              sut stackv4 (Eio.Stream.add error_mbox);
              Eio.Time.sleep clock 0.1);
          ];
        VNETIF_STACK.Stackv4.disconnect stackv4;
        Eio.Promise.resolve disconnect_raw_stack ();
        None);
      (fun () -> Some (Eio.Stream.take error_mbox));
    ]
  |> function
  | None -> ()
  | Some err -> Alcotest.fail err

(* Helper functions *)
let reply_id_from ~src ~dst data =
  let sport = Tcp_wire.get_tcp_src_port data in
  let dport = Tcp_wire.get_tcp_dst_port data in
  WIRE.v ~dst_port:sport ~dst:src ~src_port:dport ~src:dst

let ack_for data =
  match Tcp_unmarshal.of_cstruct data with
  | Error s -> Alcotest.fail ("attempting to ack data: " ^ s)
  | Ok (packet, data) ->
      let open Tcp.Tcp_packet in
      let data_len =
        Sequence.of_int
          (Cstruct.length data
          + (if packet.fin then 1 else 0)
          + if packet.syn then 1 else 0)
      in
      let sequence = packet.sequence in
      let ack_n = Sequence.(add sequence data_len) in
      ack_n

let ack data = Some (ack_for data)
let ack_in_future data off = Some Sequence.(add (ack_for data) (of_int off))
let ack_from_past data off = Some Sequence.(sub (ack_for data) (of_int off))

let fail_result_not_expected fail = function
  | Error _err -> fail "error not expected"
  | Ok `Eof -> fail "eof"
  | Ok (`Data data) ->
      Alcotest.fail
        (Format.asprintf "data not expected but received: %a" Cstruct.hexdump_pp
           data)

(* Test scenarios *)

(* Common sut: able to connect, connection not reset, no data received *)
let sut_connects_and_remains_connected ~clock stack fail_callback =
  let conn =
    VNETIF_STACK.Stackv4.TCPV4.create_connection
      (VNETIF_STACK.Stackv4.tcpv4 stack)
  in
  let flow = conn (server_ip, 80) in
  (* We must remain blocked on read, connection shouldn't be terminated.
   * If after half second that remains true, assume test succeeds *)
  try
    Eio.Time.with_timeout_exn clock 0.5 (fun () ->
        let buffer = Cstruct.create 1024 in
        let _ = Eio.Flow.read flow buffer in
        fail_result_not_expected fail_callback (Ok (`Data buffer)))
  with Eio.Time.Timeout -> ()

let blind_rst_on_syn_scenario =
  let fsm ip state ~src ~dst data =
    match state with
    | `WAIT_FOR_SYN ->
        let syn = Tcp_wire.get_syn data in
        if syn then (
          let id = reply_id_from ~src ~dst data in
          (* This -blind- reset must be ignored because of invalid ack. *)
          WIRE.xmit ~ip id ~rst:true ~rx_ack:(ack_from_past data 1)
            ~seq:(Sequence.of_int32 0l) ~window ~options (Cstruct.create 0);
          (* The syn-ack must be received and connection established *)
          WIRE.xmit ~ip id ~syn:true ~rx_ack:(ack data)
            ~seq:(Sequence.of_int32 0l) ~window ~options (Cstruct.create 0);
          Fsm_next `WAIT_FOR_ACK)
        else Fsm_error "Expected initial syn request"
    | `WAIT_FOR_ACK ->
        if Tcp_wire.get_ack data then Fsm_done
        else Fsm_error "Expected final ack of three step dance"
    | `END -> Fsm_error "nothing expected"
  in
  ((`WAIT_FOR_SYN, fsm), sut_connects_and_remains_connected)

let connection_refused_scenario =
  let fsm ip state ~src ~dst data =
    match state with
    | `WAIT_FOR_SYN ->
        let syn = Tcp_wire.get_syn data in
        if syn then (
          let id = reply_id_from ~src ~dst data in
          (* refused *)
          WIRE.xmit ~ip id ~rst:true ~rx_ack:(ack data)
            ~seq:(Sequence.of_int32 0l) ~window ~options (Cstruct.create 0);
          Fsm_done)
        else Fsm_error "Expected initial syn request"
  in
  let sut ~clock:_ stack _fail =
    let conn =
      VNETIF_STACK.Stackv4.TCPV4.create_connection
        (VNETIF_STACK.Stackv4.tcpv4 stack)
    in
    (* connection must be rejected *)
    expect_exception Tcpip.Tcp.Refused "connect" conn (server_ip, 80) |> ignore
  in
  ((`WAIT_FOR_SYN, fsm), sut)

let blind_rst_on_established_scenario =
  let fsm ip state ~src ~dst data =
    match state with
    | `WAIT_FOR_SYN ->
        let syn = Tcp_wire.get_syn data in
        if syn then (
          let id = reply_id_from ~src ~dst data in
          WIRE.xmit ~ip id ~syn:true ~rx_ack:(ack data)
            ~seq:(Sequence.of_int32 0l) ~window ~options (Cstruct.create 0);
          Fsm_next `WAIT_FOR_ACK)
        else Fsm_error "Expected initial syn request"
    | `WAIT_FOR_ACK ->
        if Tcp_wire.get_ack data then (
          (* This -blind- reset is acceptable, but don't exactly match the next sequence (we started at 0, this is 10).
           * Must trigger a challenge ack and not tear down the connection *)
          let id = reply_id_from ~src ~dst data in
          WIRE.xmit ~ip id ~rst:true ~rx_ack:None ~seq:(Sequence.of_int32 10l)
            ~window ~options (Cstruct.create 0);
          Fsm_next `WAIT_FOR_CHALLENGE)
        else Fsm_error "Expected final ack of three way handshake"
    | `WAIT_FOR_CHALLENGE ->
        if Tcp_wire.get_ack data && Tcp_wire.get_tcp_ack_number data = 1l then
          Fsm_done
        else Fsm_error "Challenge ack expected"
  in
  ((`WAIT_FOR_SYN, fsm), sut_connects_and_remains_connected)

let rst_on_established_scenario =
  let fsm ip state ~src ~dst data =
    match state with
    | `WAIT_FOR_SYN ->
        let syn = Tcp_wire.get_syn data in
        if syn then (
          let id = reply_id_from ~src ~dst data in
          WIRE.xmit ~ip id ~syn:true ~rx_ack:(ack data)
            ~seq:(Sequence.of_int32 0l) ~window ~options (Cstruct.create 0);
          Fsm_next `WAIT_FOR_ACK)
        else Fsm_error "Expected initial syn request"
    | `WAIT_FOR_ACK ->
        if Tcp_wire.get_ack data then (
          let id = reply_id_from ~src ~dst data in
          (* This reset is acceptable and exactly in sequence. Must trigger a reset on the other end *)
          WIRE.xmit ~ip id ~rst:true ~rx_ack:None ~seq:(Sequence.of_int32 1l)
            ~window ~options (Cstruct.create 0);
          Fsm_done)
        else Fsm_error "Expected final ack of three step dance"
  in

  let sut ~clock:_ stack fail_callback =
    let conn =
      VNETIF_STACK.Stackv4.TCPV4.create_connection
        (VNETIF_STACK.Stackv4.tcpv4 stack)
    in
    let flow = conn (server_ip, 80) in
    let buf = Cstruct.create_unsafe 1024 in
    match Eio.Flow.read flow buf with
    | exception End_of_file ->
        (* This is the expected when the other end resets *)
        ()
    | _ -> fail_result_not_expected fail_callback (Ok (`Data buf))
  in
  ((`WAIT_FOR_SYN, fsm), sut)

let blind_syn_on_established_scenario =
  let fsm ip state ~src ~dst data =
    match state with
    | `WAIT_FOR_SYN ->
        let syn = Tcp_wire.get_syn data in
        if syn then (
          let id = reply_id_from ~src ~dst data in
          WIRE.xmit ~ip id ~syn:true ~rx_ack:(ack data)
            ~seq:(Sequence.of_int32 0l) ~window ~options (Cstruct.create 0);
          Fsm_next `WAIT_FOR_ACK)
        else Fsm_error "Expected initial syn request"
    | `WAIT_FOR_ACK ->
        if Tcp_wire.get_ack data then (
          let id = reply_id_from ~src ~dst data in

          (* This -blind- syn should trigger a challenge ack and not
             tear down the connection *)
          WIRE.xmit ~ip id ~syn:true ~rx_ack:None ~seq:(Sequence.of_int32 10l)
            ~window ~options (Cstruct.create 0);
          Fsm_next `WAIT_FOR_CHALLENGE)
        else Fsm_error "Expected final ack of three step dance"
    | `WAIT_FOR_CHALLENGE ->
        if Tcp_wire.get_ack data && Tcp_wire.get_tcp_ack_number data = 1l then
          Fsm_done
        else Fsm_error "Challenge ack expected"
  in
  ((`WAIT_FOR_SYN, fsm), sut_connects_and_remains_connected)

let blind_data_injection_scenario =
  let page = Cstruct.create 512 in
  let fsm ip state ~src ~dst data =
    match state with
    | `WAIT_FOR_SYN ->
        let syn = Tcp_wire.get_syn data in
        if syn then (
          let id = reply_id_from ~src ~dst data in
          WIRE.xmit ~ip id ~syn:true ~rx_ack:(ack data)
            ~seq:(Sequence.of_int32 1000000l)
            ~window ~options (Cstruct.create 0);
          Fsm_next `WAIT_FOR_ACK)
        else Fsm_error "Expected initial syn request"
    | `WAIT_FOR_ACK ->
        if Tcp_wire.get_ack data then (
          let id = reply_id_from ~src ~dst data in
          (* This -blind- data should trigger a challenge ack and not
             tear down the connection *)
          let invalid_ack = ack_from_past data (window + 100) in
          WIRE.xmit ~ip id ~rx_ack:invalid_ack
            ~seq:(Sequence.of_int32 1000001l)
            ~window ~options page;
          Fsm_next `WAIT_FOR_CHALLENGE)
        else Fsm_error "Expected final ack of three step dance"
    | `WAIT_FOR_CHALLENGE ->
        if Tcp_wire.get_ack data && Tcp_wire.get_tcp_ack_number data = 1000001l
        then Fsm_done
        else Fsm_error "Challenge ack expected"
  in
  ((`WAIT_FOR_SYN, fsm), sut_connects_and_remains_connected)

let data_repeated_ack_scenario =
  (* This is the just data transmission with ack in the past but within the acceptable window *)
  let page = Cstruct.create 512 in
  let fsm ip state ~src ~dst data =
    match state with
    | `WAIT_FOR_SYN ->
        let syn = Tcp_wire.get_syn data in
        if syn then (
          let id = reply_id_from ~src ~dst data in
          WIRE.xmit ~ip id ~syn:true ~rx_ack:(ack data)
            ~seq:(Sequence.of_int32 1000000l)
            ~window ~options (Cstruct.create 0);
          Fsm_next `WAIT_FOR_ACK)
        else Fsm_error "Expected initial syn request"
    | `WAIT_FOR_ACK ->
        if Tcp_wire.get_ack data then (
          let id = reply_id_from ~src ~dst data in
          (* Ack is old but within the acceptable window. *)
          let valid_ack = ack_from_past data (window - 100) in
          WIRE.xmit ~ip id ~rx_ack:valid_ack
            ~seq:(Sequence.of_int32 1000001l)
            ~window ~options page;
          Fsm_next `WAIT_FOR_DATA_ACK)
        else Fsm_error "Expected final ack of three step dance"
    | `WAIT_FOR_DATA_ACK ->
        if
          Tcp_wire.get_ack data
          && Tcp_wire.get_tcp_ack_number data
             = Int32.(add 1000001l (of_int (Cstruct.length page)))
        then Fsm_done
        else Fsm_error "Ack for data expected"
  in

  let sut ~clock:_ stack fail_callback =
    let conn =
      VNETIF_STACK.Stackv4.TCPV4.create_connection
        (VNETIF_STACK.Stackv4.tcpv4 stack)
    in
    let flow = conn (server_ip, 80) in
    (* We should receive the data *)
    let buffer = Cstruct.create 1024 in
    match Eio.Flow.read flow buffer with
    | _ -> ()
    | exception End_of_file -> fail_result_not_expected fail_callback (Ok `Eof)
  in
  ((`WAIT_FOR_SYN, fsm), sut)

let run_test pcap_file ((initial_state, fsm), sut) () =
  Eio_linux.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let dir = Eio.Stdenv.fs env in
  Common.switch_run_cancel_on_return @@ fun sw ->
  let backend = VNETIF_STACK.create_backend ~sw ~clock () in
  VNETIF_STACK.record_pcap ~dir backend pcap_file
    (run ~sw ~clock backend (initial_state, fsm) (sut ~clock));
  Eio.Switch.fail sw Not_found

let suite =
  [
    ( "blind rst to syn_sent",
      `Quick,
      run_test "tcp_blind_rst_on_syn.pcap" blind_rst_on_syn_scenario );
    ( "connection refused",
      `Quick,
      run_test "tcp_connection_refused.pcap" connection_refused_scenario );
    ( "blind rst on established",
      `Quick,
      run_test "tcp_blind_rst_on_established.pcap"
        blind_rst_on_established_scenario );
    ( "rst on established",
      `Quick,
      run_test "tcp_rst_on_established.pcap" rst_on_established_scenario );
    ( "blind syn on established",
      `Quick,
      run_test "tcp_blind_syn_on_established.pcap"
        blind_syn_on_established_scenario );
    ( "blind data injection",
      `Quick,
      run_test "tcp_blind_data_injection.pcap" blind_data_injection_scenario );
    ( "data repeated ack",
      `Quick,
      run_test "tcp_data_repeated_ack.pcap" data_repeated_ack_scenario );
  ]
