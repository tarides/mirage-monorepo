let time_reduction_factor = 600.

let fast_clock clock: Eio.Time.clock =
  object
    method now = clock#now *. time_reduction_factor

    method sleep_until f = clock#sleep_until (f /. time_reduction_factor)
  end

module B = Basic_backend.Make
module V = Vnetif.Make(B)
module E = Ethernet.Make(V)
module A = Arp.Make(E)

let src = Logs.Src.create "test_arp" ~doc:"Mirage ARP tester"
module Log = (val Logs.src_log src : Logs.LOG)

type arp_stack = {
  backend : B.t;
  netif: V.t;
  ethif: E.t;
  arp: A.t;
}

let first_ip = Ipaddr.V4.of_string_exn "192.168.3.1"
let second_ip = Ipaddr.V4.of_string_exn "192.168.3.10"
let sample_mac = Macaddr.of_string_exn "10:9a:dd:c0:ff:ee"

let packet = (module Arp_packet : Alcotest.TESTABLE with type t = Arp_packet.t)

let ip =
  let module M = struct
    type t = Ipaddr.V4.t
    let pp = Ipaddr.V4.pp
    let equal p q = (Ipaddr.V4.compare p q) = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let macaddr =
  let module M = struct
    type t = Macaddr.t
    let pp = Macaddr.pp
    let equal p q = (Macaddr.compare p q) = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let header_size = Ethernet.Packet.sizeof_ethernet
let size = Arp_packet.size

let check_header ~message expected actual =
  Alcotest.(check packet) message expected actual

let fail = Alcotest.fail
let failf fmt = Fmt.kstr (fun s -> Alcotest.fail s) fmt

let timeout ~time ~clock t =
  let msg = Printf.sprintf "Timed out: didn't complete in %d milliseconds" time in
  Eio.Fiber.any [ 
    t; 
    fun () -> 
      Eio.Time.sleep clock (Float.of_int time /. 1000.);
      fail msg; 
  ]

let check_response expected buf =
  match Arp_packet.decode buf with
  | Error s -> Alcotest.fail (Fmt.to_to_string Arp_packet.pp_error s)
  | Ok actual ->
    Alcotest.(check packet) "parsed packet comparison" expected actual

let check_ethif_response expected buf =
  let open Ethernet.Packet in
  match of_cstruct buf with
  | Error s -> Alcotest.fail s
  | Ok ({ethertype; _}, arp) ->
    match ethertype with
    | `ARP -> check_response expected arp
    | _ -> Alcotest.fail "Ethernet packet with non-ARP ethertype"

let garp source_mac source_ip =
  let open Arp_packet in
  {
    operation = Request;
    source_mac;
    target_mac = Macaddr.of_octets_exn "\000\000\000\000\000\000";
    source_ip;
    target_ip = source_ip;
  }

let fail_on_receipt netif buf =
  Alcotest.fail (Format.asprintf "received traffic when none was expected on interface %a: %a"
	  Macaddr.pp (V.mac netif) Cstruct.hexdump_pp buf)

let single_check netif expected =
  V.listen netif ~header_size (fun buf ->
      match Ethernet.Packet.of_cstruct buf with
      | Error _ -> failwith "sad face"
      | Ok (_, payload) ->
        check_response expected payload; V.disconnect netif) |> ignore

(*  { Ethernet_packet.source = arp.source_mac;
      destination = arp.target_mac;
      ethertype = `ARP;
    } *)

let arp_reply ~from_netif ~to_netif ~from_ip ~to_ip =
  let open Arp_packet in
  let buffer = Cstruct.create Arp_packet.size in
  let a =
    { operation = Reply;
      source_mac = V.mac from_netif;
      target_mac = V.mac to_netif;
      source_ip = from_ip;
      target_ip = to_ip}
  in
  encode_into a buffer ;
  buffer

let arp_request ~from_netif ~to_mac ~from_ip ~to_ip =
  let open Arp_packet in
  let buffer = Cstruct.create Arp_packet.size in
  let a =
    { operation = Request;
      source_mac = V.mac from_netif;
      target_mac = to_mac;
      source_ip = from_ip;
      target_ip = to_ip}
  in
  encode_into a buffer;
  buffer

let get_arp ~sw ~clock ?backend () =
  let backend = match backend with
    | None -> B.create ~use_async_readers:sw ()
    | Some b -> b
  in
  let netif = V.connect backend in
  let ethif = E.connect netif in
  let arp = A.connect ~sw ~clock:(fast_clock clock) ethif in
  { backend; netif; ethif; arp }

(* we almost always want two stacks on the same backend *)
let two_arp ~sw ~clock () =
  let first = get_arp ~sw ~clock () in
  let second = get_arp ~sw ~clock ~backend:first.backend () in
  (first, second)

(* ...but sometimes we want three *)
let three_arp ~sw ~clock () =
  let first = get_arp ~sw ~clock () in
  let second = get_arp ~sw ~clock ~backend:first.backend () in
  let third = get_arp ~sw ~clock ~backend:first.backend () in
  (first, second, third)

let query_or_die arp ip expected_mac =
  match A.query arp ip with
  | exception Arp.Timeout ->
    Log.warn (fun f -> f "Timeout querying %a. Table contents: %a"
                 Ipaddr.V4.pp ip A.pp arp);
    fail "ARP query failed when success was mandatory";
  | mac ->
    Alcotest.(check macaddr) "mismatch for expected query value" expected_mac mac

let query_and_no_response arp ip =
  match A.query arp ip with
  | exception Arp.Timeout ->
    Log.warn (fun f -> f "Timeout querying %a. Table contents: %a" Ipaddr.V4.pp ip A.pp arp)
  | _ -> failf "expected nothing, found something in cache"

let set_and_check ~listener ~claimant ip =
  A.set_ips claimant.arp [ ip ];
  Log.debug (fun f -> f "Set IP for %a to %a" Macaddr.pp (V.mac claimant.netif) Ipaddr.V4.pp ip);
  Logs.debug (fun f -> f "Listener table contents after IP set on claimant: %a" A.pp listener);
  query_or_die listener ip (V.mac claimant.netif)

let start_arp_listener stack () =
  let noop = (ignore) in
  Log.debug (fun f -> f "starting arp listener for %a" Macaddr.pp (V.mac stack.netif));
  let arpv4 frame =
    Log.debug (fun f -> f "frame received for arpv4");
    A.input stack.arp frame
  in
  E.input ~arpv4 ~ipv4:noop ~ipv6:noop stack.ethif

let not_in_cache ~clock ~listen probe arp ip =
  Eio.Fiber.any [
    (fun () -> single_check listen probe);
    fun () -> 
      Eio.Time.sleep clock 0.1;
      match A.query arp ip with
      | _ -> failf "entry in cache when it shouldn't be %a" Ipaddr.V4.pp ip
      | exception Arp.Timeout -> ()
  ]

let set_ip_sends_garp ~sw ~clock () =
  let (speak, listen) = two_arp ~sw ~clock () in
  let emit_garp () =
    Eio.Time.sleep clock 0.1;
    A.set_ips speak.arp [ first_ip ];
    Alcotest.(check (list ip)) "garp emitted when setting ip" [ first_ip ] (A.get_ips speak.arp)
  in
  let expected_garp = garp (V.mac speak.netif) first_ip in
  timeout ~clock ~time:500 (fun () ->
  Eio.Fiber.all [
    (fun () -> single_check listen.netif expected_garp);
    emit_garp;
  ]);
  (* now make sure we have consistency when setting *)
  A.set_ips speak.arp [];
  Alcotest.(check (slist ip Ipaddr.V4.compare)) "list of bound IPs on initialization" [] (A.get_ips speak.arp);
  A.set_ips speak.arp [ first_ip; second_ip ];
  Alcotest.(check (slist ip Ipaddr.V4.compare)) "list of bound IPs after setting two IPs"
    [ first_ip; second_ip ] (A.get_ips speak.arp)

let add_get_remove_ips ~sw ~clock () =
  let stack = get_arp ~sw ~clock () in
  let check str expected =
    Alcotest.(check (list ip)) str expected (A.get_ips stack.arp)
  in
  check "bound ips is an empty list on startup" [];
  A.set_ips stack.arp [ first_ip; first_ip ];
  check "set ips with duplicate elements result in deduplication" [first_ip];
  A.remove_ip stack.arp first_ip;
  check "ip list is empty after removing only ip" [];
  A.remove_ip stack.arp first_ip;
  check "ip list is empty after removing from empty list" [];
  A.add_ip stack.arp first_ip;
  check "first ip is the only member of the set of bound ips" [first_ip];
  A.add_ip stack.arp first_ip;
  check "adding ips is idempotent" [first_ip]

let input_single_garp ~sw ~clock () =
  let (listen, speak) = two_arp ~sw ~clock () in
  (* set the IP on speak_arp, which should cause a GARP to be emitted which
     listen_arp will hear and cache. *)
  let one_and_done buf =
    let arpbuf = Cstruct.shift buf 14 in
    A.input listen.arp arpbuf;
    V.disconnect listen.netif
  in
  timeout ~clock ~time:500 (fun () ->
    Eio.Fiber.all [
      (fun () -> V.listen listen.netif ~header_size one_and_done |> ignore);
      (fun () -> 
        Eio.Time.sleep clock 0.1;
        Eio.Fiber.fork ~sw (fun () -> A.query listen.arp first_ip |> ignore);
        A.set_ips speak.arp [ first_ip ])
    ]);
  (* try a lookup of the IP set by speak.arp, and fail if this causes listen_arp
     to block or send an ARP query -- listen_arp should answer immediately from
     the cache.  An attempt to resolve via query will result in a timeout, since
     speak.arp has no listener running and therefore won't answer any arp
     who-has requests. *)
    timeout ~clock ~time:500 (fun () -> 
      query_or_die listen.arp first_ip (V.mac speak.netif))

let input_single_unicast ~sw ~clock () =
  let (listen, speak) = two_arp ~sw ~clock() in
  (* contrive to make a reply packet for the listener to hear *)
  let for_listener = arp_reply
      ~from_netif:speak.netif ~to_netif:listen.netif
      ~from_ip:first_ip ~to_ip:second_ip
  in
  let listener = start_arp_listener listen () in
  timeout ~clock ~time:500 (fun () ->
  Eio.Fiber.any [
    (fun () -> V.listen listen.netif ~header_size listener |> ignore);
    fun () ->
      Eio.Time.sleep clock 0.002;
      E.writev speak.ethif (V.mac listen.netif) `ARP [for_listener] |> ignore;
      query_and_no_response listen.arp first_ip
  ])

let input_resolves_wait ~sw ~clock () =
  let (listen, speak) = two_arp ~sw ~clock() in
  (* contrive to make a reply packet for the listener to hear *)
  let for_listener = arp_reply ~from_netif:speak.netif ~to_netif:listen.netif
                         ~from_ip:first_ip ~to_ip:second_ip
  in
  (* initiate query when the cache is empty.  On resolution, fail for a timeout
     and test the MAC if resolution was successful, then disconnect the
     listening interface to ensure the test terminates.
     Fail with a timeout message if the whole thing takes more than 5s. *)
  let listener = start_arp_listener listen () in
  let query_then_disconnect () =
    query_or_die listen.arp first_ip (V.mac speak.netif);
    V.disconnect listen.netif
  in
  timeout ~clock ~time:5000 (fun () ->
    Eio.Fiber.all [
      (fun () -> V.listen listen.netif ~header_size listener |> ignore);
      query_then_disconnect;
      (fun () ->
        Eio.Time.sleep clock 0.001;
        E.writev speak.ethif (V.mac listen.netif) `ARP [for_listener])
    ]
  )

let unreachable_times_out ~sw ~clock () =
  let speak = get_arp ~sw ~clock () in
  match A.query speak.arp first_ip with
  | _ -> failf "query claimed success when impossible for %a" Ipaddr.V4.pp first_ip
  | exception Arp.Timeout -> ()

let input_replaces_old ~sw ~clock () =
  let (listen, claimant_1, claimant_2) = three_arp ~sw ~clock () in
  (* query for IP to accept responses *)
  Eio.Fiber.fork ~sw (fun () -> A.query listen.arp first_ip |> ignore) ;
  Eio.Fiber.fork ~sw (fun () ->
      Log.debug (fun f -> f "arp listener started");
      V.listen listen.netif ~header_size (start_arp_listener listen ()) |> ignore);
  timeout ~clock ~time:2000 (fun () ->
    set_and_check ~listener:listen.arp ~claimant:claimant_1 first_ip;
    set_and_check ~listener:listen.arp ~claimant:claimant_2 first_ip;
    V.disconnect listen.netif
    )

let entries_expire ~sw ~clock () =
  let (listen, speak) = two_arp ~sw ~clock () in
  A.set_ips listen.arp [ second_ip ];
  (* here's what we expect listener to emit once its cache entry has expired *)
  let expected_arp_query =
    Arp_packet.({operation = Request;
                 source_mac = V.mac listen.netif;
                 target_mac = Macaddr.broadcast;
                 source_ip = second_ip; target_ip = first_ip})
  in
  (* query for IP to accept responses *)
  Eio.Fiber.fork ~sw (fun () -> A.query listen.arp first_ip |> ignore) ;
  Eio.Fiber.fork ~sw (fun () -> V.listen listen.netif ~header_size (start_arp_listener listen ()) |> ignore);
  let test () =
    Eio.Time.sleep clock 0.010;
    set_and_check ~listener:listen.arp ~claimant:speak first_ip;
    (* sleep for 5s to make sure we hit `tick` often enough *)
    Eio.Time.sleep clock 5.;
    Eio.traceln "%a" A.pp listen.arp;
    
    (* asking now should generate a query *)
    not_in_cache ~clock ~listen:speak.netif expected_arp_query listen.arp first_ip
  in
  timeout ~clock ~time:7000 test

(* RFC isn't strict on how many times to try, so we'll just say any number
   greater than 1 is fine *)
let query_retries ~sw ~clock () =
  let (listen, speak) = two_arp ~sw ~clock() in
  let expected_query = Arp_packet.({source_mac = V.mac speak.netif;
                                    target_mac = Macaddr.broadcast;
                                    source_ip = Ipaddr.V4.any;
                                    target_ip = first_ip;
                                    operation = Request;})
  in
  let how_many = ref 0 in
  let listener buf =
    check_ethif_response expected_query buf;
    if !how_many = 0 then
      how_many := !how_many + 1
    else 
      V.disconnect listen.netif
  in
  let ask () =
    match A.query speak.arp first_ip with
    | exception Arp.Timeout -> failf "Received error before >1 query: %s" (Printexc.to_string Arp.Timeout)
    | _ -> failf "got result from query for %a, erroneously" Ipaddr.V4.pp first_ip
  in
  Eio.Fiber.any [
    (fun () -> V.listen listen.netif ~header_size listener |> ignore);
    (fun () -> Eio.Time.sleep clock 0.002; ask ());
    (fun () -> Eio.Time.sleep clock 6.;
      fail "query didn't succeed or fail within 6s")
  ]

(* requests for us elicit a reply *)
let requests_are_responded_to ~sw ~clock () =
  let (answerer_ip, inquirer_ip) = (first_ip, second_ip) in
  let (inquirer, answerer) = two_arp ~sw ~clock  () in
  (* neither has a listener set up when we set IPs, so no GARPs in the cache *)
  A.add_ip answerer.arp answerer_ip;
  A.add_ip inquirer.arp inquirer_ip;
  let request = arp_request ~from_netif:inquirer.netif ~to_mac:Macaddr.broadcast
      ~from_ip:inquirer_ip ~to_ip:answerer_ip
  in
  let expected_reply =
    Arp_packet.({ operation = Reply;
                  source_mac = V.mac answerer.netif;
                  target_mac = V.mac inquirer.netif;
                  source_ip = answerer_ip; target_ip = inquirer_ip})
  in
  let listener close_netif buf =
    check_ethif_response expected_reply buf;
    V.disconnect close_netif
  in
  let arp_listener () =
    V.listen answerer.netif ~header_size (start_arp_listener answerer ()) |> ignore
  in
  timeout ~clock ~time:1000 (fun () ->
    Eio.Fiber.any [
      (* listen for responses and check them against an expected result *)
      (fun () -> V.listen inquirer.netif ~header_size (listener inquirer.netif) |> ignore);
      (* start the usual ARP listener, which should respond to requests *)
      arp_listener;
      (* send a request for the ARP listener to respond to *)
      (fun () -> 
        Eio.Time.sleep clock 0.100;
        E.writev inquirer.ethif Macaddr.broadcast `ARP [request] |> ignore;
        Eio.Time.sleep clock 0.100;
        V.disconnect answerer.netif)
    ];
  )

let requests_not_us ~sw ~clock () =
  let (answerer_ip, inquirer_ip) = (first_ip, second_ip) in
  let (inquirer, answerer) = two_arp ~sw ~clock () in
  A.add_ip answerer.arp answerer_ip;
  A.add_ip inquirer.arp inquirer_ip;
  let ask ip =
    let open Arp_packet in
    let buf = Cstruct.create size in
    encode_into
      { operation = Request;
        source_mac = V.mac inquirer.netif; target_mac = Macaddr.broadcast;
        source_ip = inquirer_ip; target_ip = ip }
      buf;
    buf
  in
  let requests = List.map ask [ inquirer_ip; Ipaddr.V4.any;
                                Ipaddr.V4.of_string_exn "255.255.255.255" ] in
  let make_requests () =
    List.iter (fun b -> E.writev inquirer.ethif Macaddr.broadcast `ARP [b] |> ignore)
      requests
  in
  let disconnect_listeners () =
    List.iter (V.disconnect) [answerer.netif; inquirer.netif]
  in
  Eio.Fiber.all [ 
    (fun () -> V.listen answerer.netif ~header_size (start_arp_listener answerer ()) |> ignore);
    (fun () -> V.listen inquirer.netif ~header_size (fail_on_receipt inquirer.netif) |> ignore);
    (fun () -> 
      make_requests ();
      Eio.Time.sleep clock 0.100;
      disconnect_listeners ())
  ]

let nonsense_requests ~sw ~clock () =
  let (answerer_ip, inquirer_ip) = (first_ip, second_ip) in
  let (answerer, inquirer, checker) = three_arp ~sw ~clock () in
  A.set_ips answerer.arp [ answerer_ip ];
  let request number =
    let open Arp_packet in
    let arp = Cstruct.create Arp_packet.size in
    encode_into
      { operation = Request;
	source_mac = V.mac inquirer.netif;
	target_mac = Macaddr.broadcast;
	source_ip = inquirer_ip;
	target_ip = answerer_ip } arp ;
    Cstruct.BE.set_uint16 arp 6 number;
    arp
  in
  let requests = List.map request [0; 3; -1; 255; 256; 257; 65536] in
  let make_requests () =
    List.iter (fun l -> E.writev inquirer.ethif Macaddr.broadcast `ARP [l] |> ignore) requests in
  let expected_probe = Arp_packet.{ operation = Request;
                                    source_mac = V.mac answerer.netif;
                                    source_ip = answerer_ip;
                                    target_mac = Macaddr.broadcast;
                                    target_ip = inquirer_ip; }
  in
  Eio.Fiber.fork ~sw (fun () -> V.listen answerer.netif ~header_size (start_arp_listener answerer ()) |> ignore);
  timeout ~clock ~time:1000 (fun () ->
    Eio.Fiber.all [
      (fun () -> V.listen inquirer.netif ~header_size (fail_on_receipt inquirer.netif) |> ignore);
      (fun () -> 
        make_requests ();
        V.disconnect inquirer.netif;
        (* not sufficient to just check to see whether we've replied; it's equally
          possible that we erroneously make a cache entry.  Make sure querying
          inquirer_ip results in an outgoing request. *)
        not_in_cache ~clock ~listen:checker.netif expected_probe answerer.arp inquirer_ip)
    ] )

let packet ~sw:_ ~clock:_ () =
  let first_mac  = Macaddr.of_string_exn "10:9a:dd:01:23:45" in
  let second_mac = Macaddr.of_string_exn "00:16:3e:ab:cd:ef" in
  let example_request =
    Arp_packet.{ operation = Request;
                 source_mac = first_mac;
                 target_mac = second_mac;
                 source_ip = first_ip;
                 target_ip = second_ip;
               }
  in
  let marshalled = Arp_packet.encode example_request in
  match Arp_packet.decode marshalled with
  | Error _ -> Alcotest.fail "couldn't unmarshal something we made ourselves"
  | Ok unmarshalled ->
    Alcotest.(check packet) "serialize/deserialize" example_request unmarshalled



let suite =
  [
    "conversions neither lose nor gain information", `Quick, packet;
    "nonsense requests are ignored", `Quick, nonsense_requests;
    "requests are responded to", `Quick, requests_are_responded_to;
    "entries expire", `Quick, entries_expire;
    "irrelevant requests are ignored", `Quick, requests_not_us;
    "set_ip sets ip, sends GARP", `Quick, set_ip_sends_garp;
    "add_ip, get_ip and remove_ip as advertised", `Quick, add_get_remove_ips;
    "GARPs are heard and not cached", `Quick, input_single_garp;
    "unsolicited unicast replies are heard and not cached", `Quick, input_single_unicast;
    "solicited unicast replies resolve pending threads", `Quick, input_resolves_wait;
    "entries are replaced with new information", `Quick, input_replaces_old;
    "unreachable IPs time out", `Quick, unreachable_times_out;
    "queries are tried repeatedly before timing out", `Quick, query_retries;
  ]

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

let () =
  (* enable logging to stdout for all modules *)
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level ~all:true (Some Logs.Debug);
  let suite =
    [ "arp", List.map (fun (d, s, f) -> d, s, run f) suite ]
  in
  Alcotest.run "arp" suite
