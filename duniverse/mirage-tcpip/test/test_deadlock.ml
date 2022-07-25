let mtu = 4000

let server_log = Logs.Src.create "test_deadlock_server" ~doc:"tcp deadlock tests: server"
module Server_log = (val Logs.src_log server_log : Logs.LOG)

let client_log = Logs.Src.create "test_deadlock_client" ~doc:"tcp deadlock tests: client"
module Client_log = (val Logs.src_log client_log : Logs.LOG)

module TCPIP =
struct
  module RANDOM = Mirage_random_test


  module MCLOCK = Mclock

  module M =
  struct
    module B      = Basic_backend.Make
    module NETIF  = Vnetif.Make(B)
    module ETHIF  = Ethernet.Make(NETIF)
    module ARPV4  = Arp.Make(ETHIF)
    module IPV4   = Static_ipv4.Make(RANDOM)(MCLOCK)(ETHIF)(ARPV4)
    module ICMPV4 = Icmpv4.Make(IPV4)
    module UDPV4  = Udp.Make(IPV4)(RANDOM)
    module TCPV4  = Tcp.Flow.Make(IPV4)(MCLOCK)(RANDOM)
    module TCPIP  = Tcpip_stack_direct.Make(RANDOM)(NETIF)(ETHIF)(ARPV4)(IPV4)(ICMPV4)(UDPV4)(TCPV4)
  end
  open M

  type stack = TCPIP.t

  let server_ip = Ipaddr.V4.of_string_exn "192.168.10.10"
  let server_cidr = Ipaddr.V4.Prefix.make 24 server_ip
  let client_ip = Ipaddr.V4.of_string_exn "192.168.10.20"
  let client_cidr = Ipaddr.V4.Prefix.make 24 client_ip

  let make ~sw ~clock ~cidr ?gateway netif =
    let ethif = ETHIF.connect netif in
    let arpv4 = ARPV4.connect ~sw ~clock ethif in
    let ipv4 = IPV4.connect ~cidr ?gateway ethif arpv4 in
    let icmpv4 = ICMPV4.connect ipv4 in
    let udpv4 = UDPV4.connect ipv4 in
    let tcpv4 = TCPV4.connect ~sw ~clock ipv4 in
    TCPIP.connect ~sw netif ethif arpv4 ipv4 icmpv4 udpv4 tcpv4

  include TCPIP

  let tcpip t = t

  let make role netif = match role with
    | `Server -> make ~cidr:server_cidr netif
    | `Client -> make ~cidr:client_cidr netif

  type conn = M.NETIF.t

  let get_stats _t =
    { Mirage_net.rx_pkts = 0l; rx_bytes = 0L;
      tx_pkts = 0l; tx_bytes = 0L;
    }

  let reset_stats _t = ()
end

let port = 10000

let test_digest ~sw ~clock netif1 netif2 =
  let client_stack = TCPIP.make ~sw ~clock `Client netif1 in
  let server_stack = TCPIP.make ~sw ~clock `Server netif2 in

  let send_data () =
    let data = Mirage_random_test.generate 100_000_000 |> Cstruct.to_string in
    let t0   = Unix.gettimeofday () in
    let flow =  TCPIP.TCPV4.create_connection
      TCPIP.(tcpv4 @@ tcpip server_stack) (TCPIP.client_ip, port) 
    in
      Server_log.debug (fun f -> f "established conn");
      let rec read_digest chunks =
        let chunk = Cstruct.create 10000 in
        match Eio.Flow.read flow chunk with
        | len -> read_digest (Cstruct.sub chunk 0 len :: chunks)
        | exception End_of_file ->
          Server_log.debug (fun f -> f "EOF");
          let dt = Unix.gettimeofday () -. t0 in
          Server_log.warn (fun f -> f "!!!!!!!!!! XXXX  needed %.2fs (%.1f MB/s)"
            dt (float (String.length data) /. dt /. 1024. ** 2.))
      in
      Eio.Fiber.any
        [ (fun () -> read_digest []);
          fun () ->
          begin
            let rec send_data data =
              if Cstruct.length data < mtu then
                (Eio.Flow.(copy (cstruct_source [data])) flow)
              else
                let sub, data = Cstruct.split data mtu in
                Eio.Fiber.any
                  [
                    (fun () -> (Eio.Flow.(copy (cstruct_source [sub])) flow));
                    (fun () -> (
                      Eio.Time.sleep clock 5.;
                      Common.failf "=========== DEADLOCK!!! ============="));
                  ];
                send_data data 
            in
            send_data @@ Cstruct.of_string data;
            Server_log.debug (fun f -> f "wrote data");
            Eio.Flow.close flow
          end
        ]
  in
  TCPIP.TCPV4.listen TCPIP.(tcpv4 (tcpip client_stack)) ~port
    (fun flow ->
       Client_log.debug (fun f -> f "client got conn");
       let rec consume () =
        let buffer = Cstruct.create 10000 in
         match Eio.Flow.read flow buffer with
         | exception End_of_file ->
           Eio.Flow.copy_string "thanks for all the fish" flow;
           Eio.Flow.close flow
         | _ ->
           (if Random.float 1.0 < 0.01 then 
            Eio.Time.sleep clock 0.01);
           consume ()
         | exception _ ->
           Client_log.debug (fun f -> f "XXXX client read error");
           Eio.Flow.close flow
       in
       consume ());
  Eio.Fiber.any
    [
      (fun () -> send_data ());
      (fun () -> TCPIP.listen @@ TCPIP.tcpip server_stack);
      (fun () -> TCPIP.listen @@ TCPIP.tcpip client_stack);
    ]

let run_vnetif ~sw ~env () =
  let clock = env#clock in
  let backend = Basic_backend.Make.create
      ~use_async_readers:sw () in
  let c1 = TCPIP.M.NETIF.connect ~size_limit:mtu backend in
  let c2 = TCPIP.M.NETIF.connect ~size_limit:mtu backend in
  test_digest ~sw ~clock c1 c2

open Common

let suite = [
  "test tcp deadlock with slow receiver", `Slow, run run_vnetif
]
