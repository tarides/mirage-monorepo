
module Make(Stack: Tcpip.Stack.V4V6) = struct
  
  let eio_ip_to_mirage_ip (str : [ `V4 | `V6 ] Eio.Net.Ipaddr.t) =
    let str = (str :> string) in
    if String.length str == 4 then Ipaddr.V4 (Ipaddr.V4.of_octets_exn str)
    else V6 (Ipaddr.V6.of_octets_exn str)

  let eio_ip_to_mirage_ip =
    Eio.Net.Ipaddr.fold ~v4:eio_ip_to_mirage_ip ~v6:eio_ip_to_mirage_ip

  let mirage_ip_to_eio_ip mirage_ip =
    Eio.Net.Ipaddr.of_raw (match mirage_ip with
      | Ipaddr.V4 ip -> Ipaddr.V4.to_octets ip
      | Ipaddr.V6 ip -> Ipaddr.V6.to_octets ip
    )

  let net stack : Eio.Net.t =
    object
      method connect ~sw =
        function
        | `Unix _ -> failwith "unix sockets are not implemented"
        | `Tcp (addr, port) ->
            let ip = eio_ip_to_mirage_ip addr in
            let flow =
              (Stack.TCP.create_connection (Stack.tcp stack) (ip, port)
                :> < Eio.Flow.two_way ; Eio.Flow.close >)
            in
            Eio.Switch.on_release sw (fun () -> Eio.Flow.close flow);
            flow

      method datagram_socket ~sw (`Udp (_addr, port)) =
        let condition = Eio.Condition.create ~label:"udp socket" () in
        let () =
          Eio.Fiber.fork ~sw @@ fun () ->
          Stack.UDP.listen (Stack.udp stack) ~port
          @@ fun ~src ~dst:_ ~src_port buffer ->
          (* the buffer should be copied ? *)
          Eio.Condition.broadcast condition
            (`Udp (mirage_ip_to_eio_ip src, src_port), buffer)
        in
        (object
           method recv buffer =
             let sockaddr, netbuf = Eio.Condition.await condition in
             let len = min (Cstruct.length netbuf) (Cstruct.length buffer) in
             Cstruct.blit netbuf 0 buffer 0 len;
             (sockaddr, len)

           method send (`Udp (addr, port)) buffer =
             let ip = eio_ip_to_mirage_ip addr in
             Stack.UDP.write ~dst:ip ~dst_port:port (Stack.udp stack) buffer
         end
          :> Eio.Net.datagram_socket)

      method listen ~reuse_addr:_ ~reuse_port:_ ~backlog ~sw =
        function
        | `Unix _ -> failwith "unix sockets are not implemented"
        | `Tcp (_addr, port) ->
            let flow_queue =
              Eio.Stream.create ~label:"tcp flow queue" backlog
            in
            let close_t, close_u =
              Eio.Promise.create ~label:"tcp listen close" ()
            in
            let () =
              Stack.TCP.listen (Stack.tcp stack) ~port @@ fun flow ->
              Eio.Stream.add flow_queue flow
            in
            Eio.Switch.on_release sw (fun () -> Stack.TCP.unlisten (Stack.tcp stack) ~port);
            let () =
              Eio.Fiber.fork ~sw @@ fun () ->
              Eio.Promise.await close_t;
              Stack.TCP.unlisten (Stack.tcp stack) ~port
            in
            (object
               method accept ~sw =
                 let flow = Eio.Stream.take flow_queue in
                 Eio.Switch.on_release sw (fun () -> Eio.Flow.close flow);
                 (* TODO: dst ?? *)
                 let addr, port = flow#dst in
                 (flow, `Tcp (mirage_ip_to_eio_ip addr, port))

               method close = Eio.Promise.resolve close_u ()

               method probe : type a. a Eio.Generic.ty -> a option =
                 function _ -> None
             end
              :> Eio.Net.listening_socket)
    end
end