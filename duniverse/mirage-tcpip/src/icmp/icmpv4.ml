module type S = sig
  type t
  val disconnect : t -> unit
  type ipaddr = Ipaddr.V4.t
  val input : t -> src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
  val write : t -> ?src:ipaddr -> dst:ipaddr -> ?ttl:int -> Cstruct.t -> unit
end

let src = Logs.Src.create "icmpv4" ~doc:"Mirage ICMPv4"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (IP : Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t) = struct

  type ipaddr = Ipaddr.V4.t

  type t = {
    ip : IP.t;
    echo_reply : bool;
  }

  let connect ip = { ip; echo_reply = true }

  let disconnect _ = ()

  let writev t ?src ~dst ?ttl bufs =
    IP.write t.ip ?src dst ?ttl `ICMP (fun _ -> 0) bufs

  let write t ?src ~dst ?ttl buf = writev t ?src ~dst ?ttl [buf]

  let input t ~src ~dst:_ buf =
    let open Icmpv4_packet in
    MProf.Trace.label "icmp_input";
    match Unmarshal.of_cstruct buf with
    | Error s ->
      Log.info (fun f ->
          f "ICMP: error parsing message from %a: %s" Ipaddr.V4.pp src s)
    | Ok (message, payload) ->
      let open Icmpv4_wire in
      match message.ty, message.subheader with
      | Echo_reply, _ ->
        Log.info (fun f ->
            f "ICMP: discarding echo reply from %a" Ipaddr.V4.pp src)
      | Destination_unreachable, _ ->
        Log.info (fun f ->
            f "ICMP: destination unreachable from %a" Ipaddr.V4.pp src)
      | Echo_request, Id_and_seq (id, seq) ->
        Log.debug (fun f ->
            f "ICMP echo-request received: %a (payload %a)"
              Icmpv4_packet.pp message Cstruct.hexdump_pp payload);
        if t.echo_reply then begin
          let icmp = {
            code = 0x00;
            ty   = Icmpv4_wire.Echo_reply;
            subheader = Id_and_seq (id, seq);
          } in
          writev t ~dst:src [ Marshal.make_cstruct icmp ~payload; payload ]
        end
      | ty, _ ->
        Log.info (fun f ->
            f "ICMP unknown ty %s from %a"
              (ty_to_string ty) Ipaddr.V4.pp src)

end
