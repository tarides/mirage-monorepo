(*
 * Copyright (c) 2010-2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Balraj Singh <bs375@cl.cam.ac.uk>
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

open Eio.Std
module Lwt = struct end

let src = Logs.Src.create "pcb" ~doc:"Mirage TCP PCB module"

module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (Ip : Tcpip.Ip.S)
    (Clock : Mirage_clock.MCLOCK)
    (Random : Mirage_random.S) =
struct
  module RXS = Segment.Rx
  module TXS = Segment.Tx (Clock)
  module ACK = Ack.Immediate
  module UTX = User_buffer.Tx (Clock)
  module WIRE = Wire.Make (Ip)
  module STATE = State
  module KEEPALIVE = Keepalive.Make (Clock)

  exception Not_ready
  type ipaddr = Ip.ipaddr

  type pcb = {
    id : WIRE.t;
    wnd : Window.t; (* Window information *)
    rxq : RXS.t; (* Received segments queue for out-of-order data *)
    txq : TXS.t; (* Transmit segments queue *)
    ack : ACK.t; (* Ack state *)
    state : State.t; (* Connection state *)
    urx : User_buffer.Rx.t; (* App rx buffer *)
    utx : UTX.t; (* App tx buffer *)
    keepalive : KEEPALIVE.t option; (* Optional TCP keepalive state *)
  }

  type flow = pcb
  type connection = flow

  type dst = <dst: ipaddr * int>

  type pcb_params = {
    tx_wnd : int;
    sequence : Sequence.t;
    options : Options.t list;
    tx_isn : Sequence.t;
    rx_wnd : int;
    rx_wnd_scaleoffer : int;
  }

  type 'a or_exn = ('a, exn) result

  type async_op =
  | New_client_connect of {
     pcb_params: pcb_params;
     id: WIRE.t;
     ack_number: Sequence.t;
     keepalive: Tcpip.Tcp.Keepalive.t option;
     wakener: connection or_exn Eio.Promise.u
  }
  | New_server_connect of {
    pcb_params: pcb_params;
    id: WIRE.t;
    keepalive: Tcpip.Tcp.Keepalive.t option;
    process: < Eio.Flow.two_way ; Eio.Flow.close; dst > -> unit
  }
  | New_connection of {
    pushf: < Eio.Flow.two_way ; Eio.Flow.close; dst > -> unit;
    newconn: connection;
  }
  | Connect_timer of {
    id: WIRE.t;
    tx_isn: Sequence.t;
    options: Options.t list;
    window: int;
  }
  | State_tick of {
    state: STATE.t;
    action: STATE.action;
  }
  | Keepalive_refresh of {
    keepalive: KEEPALIVE.t
  }
  | RXS_input of {
    rxq: RXS.t;
    segment: RXS.segment;
  }

  type t = {
    ip : Ip.t;
    clock : Eio.Time.clock;
    operations : async_op Eio.Stream.t;
    listeners :
      ( int,
        Tcpip.Tcp.Keepalive.t option
        * (< Eio.Flow.two_way ; Eio.Flow.close; dst > -> unit) )
      Hashtbl.t;
    mutable active : bool;
    mutable localport : int;
    channels : (WIRE.t, connection) Hashtbl.t;
    (* server connections the process of connecting - SYN-ACK sent
       waiting for ACK *)
    listens :
      ( WIRE.t,
        Sequence.t
        * ((< Eio.Flow.two_way ; Eio.Flow.close; dst > -> unit) * connection) )
      Hashtbl.t;
    (* clients in the process of connecting *)
    connects :
      ( WIRE.t,
        connection or_exn Eio.Promise.u
        * Sequence.t
        * Tcpip.Tcp.Keepalive.t option )
      Hashtbl.t;
  }


  let listen t ~port ?keepalive cb =
    if port < 0 || port > 65535 then
      raise (Invalid_argument (Printf.sprintf "invalid port number (%d)" port))
    else Hashtbl.replace t.listeners port (keepalive, cb)

  let unlisten t ~port = Hashtbl.remove t.listeners port

  let _pp_pcb fmt pcb =
    Format.fprintf fmt "id=[%a] state=[%a]" WIRE.pp pcb.id State.pp pcb.state

  let pp_stats fmt t =
    Format.fprintf fmt "[channels=%d listens=%d connects=%d]"
      (Hashtbl.length t.channels)
      (Hashtbl.length t.listens)
      (Hashtbl.length t.connects)

  let log_with_stats name t = Log.info (fun fmt -> fmt "%s: %a" name pp_stats t)
  let wscale_default = 2

  module Tx = struct
    (* Output a TCP packet, and calculate some settings from a state descriptor *)
    let xmit_pcb ip id ~flags ~wnd ~options ~seq (datav : Cstruct.t) =
      let window = Int32.to_int (Window.rx_wnd_unscaled wnd) in
      let rx_ack = Some (Window.rx_nxt wnd) in
      let syn = match flags with Segment.Syn -> true | _ -> false in
      let fin = match flags with Segment.Fin -> true | _ -> false in
      let rst = match flags with Segment.Rst -> true | _ -> false in
      let psh = match flags with Segment.Psh -> true | _ -> false in
      WIRE.xmit ~ip id ~syn ~fin ~rst ~psh ~rx_ack ~seq ~window ~options datav

    (* Output an RST response when we dont have a PCB *)
    let send_rst { ip; _ } id ~sequence ~ack_number ~syn ~fin =
      let datalen =
        Int32.add (if syn then 1l else 0l) (if fin then 1l else 0l)
      in
      let window = 0 in
      let options = [] in
      let seq = ack_number in
      let rx_ack = Some Sequence.(add sequence (of_int32 datalen)) in
      WIRE.xmit ~ip id ~rst:true ~rx_ack ~seq ~window ~options
        (Cstruct.create 0)

    (* Output a SYN packet *)
    let send_syn { ip; _ } id ~tx_isn ~options ~window =
      WIRE.xmit ~ip id ~syn:true ~rx_ack:None ~seq:tx_isn ~window ~options
        (Cstruct.create 0)

    (* Queue up an immediate close segment *)
    let close t pcb =
      Log.info (fun f -> f "Closing connection %a" WIRE.pp pcb.id);
      match State.state pcb.state with
      | State.Established | State.Close_wait ->
          UTX.wait_for_flushed pcb.utx;
          let { wnd; _ } = pcb in
          Eio.Stream.add t.operations 
            (State_tick {
              action = State.Send_fin (Window.tx_nxt wnd);
              state = pcb.state
            });
          TXS.output ~flags:Segment.Fin pcb.txq (Cstruct.create 0)
      | _ ->
          Log.info (fun fmt ->
              fmt "TX.close: close requested but no action needed, state=%a"
                State.pp pcb.state);
          ()

    (* Thread that transmits ACKs in response to received packets,
       thus telling the other side that more can be sent, and
       also data from the user transmit queue *)
    let thread t pcb ~send_ack ~rx_ack =
      let { wnd; ack; _ } = pcb in

      (* Transmit an empty ack when prompted by the Ack thread *)
      let rec send_empty_ack () =
        let _ = Eio.Stream.take send_ack in
        let ack_number = Window.rx_nxt wnd in
        let flags = Segment.No_flags in
        let options = [] in
        let seq = Window.tx_nxt wnd in
        ACK.transmit ack ack_number;
        xmit_pcb t.ip pcb.id ~flags ~wnd ~options ~seq (Cstruct.create 0)
        |> ignore;
        (* TODO: what to do if sending failed.  Ignoring
                 * errors gives us the same behavior as if the packet
                 * was lost in transit *)
        send_empty_ack ()
      in
      (* When something transmits an ACK, tell the delayed ACK thread *)
      let rec notify () =
        let ack_number = Eio.Stream.take rx_ack in
        ACK.transmit ack ack_number;
        notify ()
      in
      Eio.Fiber.both 
        (fun () -> 
          Eio.Private.Ctf.label "tcpip.flow.tx.send_empty_ack";
          send_empty_ack ()) 
        (fun () -> 
          Eio.Private.Ctf.label "tcpip.flow.tx.notify";
          notify ())
  end

  module Rx = struct
    (* Process an incoming TCP packet that has an active PCB *)
    let input t parsed pcb =
      let { rxq; _ } = pcb in
      (* The connection is alive! *)
      (match pcb.keepalive with
      | None -> ()
      | Some keepalive -> 
        Eio.Stream.add t.operations (Keepalive_refresh {keepalive})
      );
      (* Coalesce any outstanding segments and retrieve ready segments *)
      Eio.Stream.add t.operations (RXS_input {
        rxq;
        segment= parsed
      })

    (* Thread that spools the data into an application receive buffer,
       and notifies the ACK subsystem that new data is here *)
    let thread ~sw pcb ~rx_data =
      let { wnd; ack; urx; _ } = pcb in
      (* Thread to monitor application receive and pass it up *)
      let rec rx_application_t () =
        let data, winadv = Eio.Stream.take rx_data in
        let signal_ack = function
          | None -> ()
          | Some winadv when Sequence.(gt winadv zero) ->
              Window.rx_advance wnd winadv;
              ACK.receive ack (Window.rx_nxt wnd)
          | Some winadv ->
              Window.rx_advance wnd winadv;
              ACK.pushack ack (Window.rx_nxt wnd)
        in
        match data with
        | None ->
            (* don't send an ACK in this case; this already happened *)
            STATE.tick ~sw pcb.state State.Recv_fin;
            User_buffer.Rx.add_r urx None
        | Some data ->
            signal_ack winadv;
            let rec queue = function
              | [] -> ()
              | hd :: tl ->
                  User_buffer.Rx.add_r urx (Some hd);
                  queue tl
            in
            queue data;
            rx_application_t ()
      in
      rx_application_t ()
  end

  module Wnd = struct
    let thread ~urx:_ ~utx ~wnd:_ ~state ~tx_wnd_update =
      (* Monitor our transmit window when updates are received
         remotely, and tell the application that new space is
         available when it is blocked *)
      let rec tx_window_t () =
        let tx_wnd = Eio.Stream.take tx_wnd_update in
        (match State.state state with
        | State.Reset -> UTX.reset utx
        | _ -> UTX.free utx tx_wnd);
        tx_window_t ()
      in
      tx_window_t ()
  end

  (* Helper function to apply function with contents of hashtbl, or
     take default action *)
  let with_hashtbl h k fn default =
    try fn (Hashtbl.find h k) with Not_found -> default k

  let hashtbl_find h k = try Some (Hashtbl.find h k) with Not_found -> None

  let clearpcb t id tx_isn =
    log_with_stats "removing pcb from connection tables" t;
    match hashtbl_find t.channels id with
    | Some _ ->
        Hashtbl.remove t.channels id;
        Stats.decr_channel ();
        Log.info (fun f -> f "removed %a from active channels" WIRE.pp id)
    | None -> (
        match hashtbl_find t.listens id with
        | Some (isn, _) ->
            if isn = tx_isn then (
              Hashtbl.remove t.listens id;
              Stats.decr_listen ();
              Log.info (fun f ->
                  f "removed %a from incomplete listen pcbs" WIRE.pp id))
        | None ->
            Log.info (fun f ->
                f "error in removing %a - no such connection" WIRE.pp id))

  let resolve_wnd_scaling options rx_wnd_scaleoffer =
    let tx_wnd_scale =
      List.fold_left
        (fun a -> function Options.Window_size_shift m -> Some m | _ -> a)
        None options
    in
    match tx_wnd_scale with
    | None -> ((0, 0), [])
    | Some tx_f ->
        ( (rx_wnd_scaleoffer, tx_f),
          [ Options.Window_size_shift rx_wnd_scaleoffer ] )


  let keepalive_cb ~sw t id wnd state urx = function
    | `SendProbe ->
        Log.info (fun f -> f "Sending keepalive on connection %a" WIRE.pp id);
        (* From https://tools.ietf.org/html/rfc1122#page-101

           > 4.2.3.6  TCP Keep-Alives
           ...
           > Such a segment generally contains SEG.SEQ =
           > SND.NXT-1 and may or may not contain one garbage octet
           > of data.  Note that on a quiet connection SND.NXT =
           > RCV.NXT, so that this SEG.SEQ will be outside the
           > window.  Therefore, the probe causes the receiver to
           > return an acknowledgment segment, confirming that the
           > connection is still live.  If the peer has dropped the
           > connection due to a network partition or a crash, it
           > will respond with a RST instead of an acknowledgment
           > segment.
        *)
        let flags = Segment.No_flags in
        let options = [] in
        let seq = Sequence.pred @@ Window.tx_nxt wnd in
        (* if the sending fails this behaves like a packet drop which will cause
            the connection to be eventually closed after the probes are sent *)
        Tx.xmit_pcb t.ip id ~flags ~wnd ~options ~seq (Cstruct.create 0)
        |> ignore;
        ()
    | `Close ->
        Log.info (fun f ->
            f "Keepalive timer expired, resetting connection %a" WIRE.pp id);
        STATE.tick ~sw state State.Recv_rst;
        (* Close the read direction *)
        User_buffer.Rx.add_r urx None

  let emitted_keepalive_warning = ref false

  let new_pcb ~sw t params id keepalive =
    let mtu_mss = Ip.mtu t.ip ~dst:(WIRE.dst id) - Tcp_wire.sizeof_tcp in
    let { tx_wnd; sequence; options; tx_isn; rx_wnd; rx_wnd_scaleoffer } =
      params
    in
    let tx_mss =
      List.fold_left
        (fun a -> function Options.MSS m -> min m mtu_mss | _ -> a)
        mtu_mss options
    in
    let (rx_wnd_scale, tx_wnd_scale), opts =
      resolve_wnd_scaling options rx_wnd_scaleoffer
    in
    (* Set up the windowing variables *)
    let rx_isn = sequence in
    (* Initialise the window handler *)
    let wnd =
      Window.t ~rx_wnd_scale ~tx_wnd_scale ~rx_wnd ~tx_wnd ~rx_isn ~tx_mss
        ~tx_isn
    in
    (* When we transmit an ACK for a received segment, rx_ack is written to *)
    let rx_ack = Eio.Stream.create 1 in
    (* When we receive an ACK for a transmitted segment, tx_ack is written to *)
    let tx_ack = Eio.Stream.create 1 in
    (* When new data is received, rx_data is written to *)
    let rx_data = Eio.Stream.create 1 in
    (* Write to this mvar to transmit an empty ACK to the remote side *)
    let send_ack = Eio.Stream.create 1 in
    (* The user application receive buffer and close notification *)
    let rx_buf_size = Window.rx_wnd wnd in
    let urx = User_buffer.Rx.create ~max_size:rx_buf_size ~wnd in
    (* The window handling thread *)
    let tx_wnd_update = Eio.Stream.create 1 in
    (* Set up transmit and receive queues *)
    let on_close () = clearpcb t id tx_isn in
    let state = State.t ~clock:t.clock ~on_close in
    let txq =
      TXS.create ~sw ~clock:t.clock ~xmit:(Tx.xmit_pcb t.ip id) ~wnd ~state
        ~rx_ack ~tx_ack ~tx_wnd_update
    in
    (* The user application transmit buffer *)
    let utx = UTX.create ~wnd ~txq ~max_size:16384l in
    let rxq = RXS.create ~rx_data ~wnd ~state ~tx_ack in
    (* Set up ACK module *)
    let ack = ACK.t ~sw ~clock:t.clock ~send_ack ~last:(Sequence.succ rx_isn) in
    (* Set up the keepalive state if requested *)
    let keepalive =
      match keepalive with
      | None -> None
      | Some config ->
          (* Only omit the warning once to avoid spamming the logs *)
          if not !emitted_keepalive_warning then (
            Log.warn (fun f ->
                f
                  "using keep-alives can cause excessive memory consumption: \
                   https://github.com/mirage/mirage-tcpip/issues/367");
            emitted_keepalive_warning := true);
          Some
            (KEEPALIVE.create ~sw ~clock:t.clock config
               (keepalive_cb ~sw t id wnd state urx))
    in
    (* Construct basic PCB in Syn_received state *)
    let pcb = { state; rxq; txq; wnd; id; ack; urx; utx; keepalive } in
    (* Compose the overall thread from the various tx/rx threads
       and the main listener function *)
    let tx_thread () = 
      Eio.Private.Ctf.label "tx thread";
      Tx.thread t pcb ~send_ack ~rx_ack 
    in
    let rx_thread () = 
      Eio.Private.Ctf.label "rx thread";
      Rx.thread ~sw pcb ~rx_data 
    in
    let wnd_thread () = 
      Eio.Private.Ctf.label "window thread";
      Wnd.thread ~utx ~urx ~wnd ~state ~tx_wnd_update 
    in
    let threads = [tx_thread; rx_thread; wnd_thread ] in
    List.iter (Eio.Fiber.fork ~sw) threads;
    (pcb, opts)

  let new_server_connection ~sw t params id pushf keepalive =
    log_with_stats "new-server-connection" t;
    let pcb, opts = new_pcb ~sw t params id keepalive in
    STATE.tick ~sw pcb.state State.Passive_open;
    STATE.tick ~sw pcb.state (State.Send_synack params.tx_isn);
    (* Add the PCB to our listens table *)
    if Hashtbl.mem t.listens id then (
      Log.info (fun f ->
          f
            "duplicate attempt to make a connection: %a .Removing the old \
             state and replacing with new attempt"
            WIRE.pp id);
      Hashtbl.remove t.listens id;
      Stats.decr_listen ());
    Hashtbl.add t.listens id (params.tx_isn, (pushf, pcb));
    Stats.incr_listen ();
    (* Queue a SYN ACK for transmission *)
    let options =
      Options.MSS (Ip.mtu t.ip ~dst:(WIRE.dst id) - Tcp_wire.sizeof_tcp) :: opts
    in
    TXS.output ~flags:Segment.Syn ~options pcb.txq (Cstruct.create 0);
    pcb

  let new_client_connection ~sw t params id ack_number keepalive =
    log_with_stats "new-client-connection" t;
    let tx_isn = params.tx_isn in
    let params = { params with tx_isn = Sequence.succ tx_isn } in
    let pcb, _ = new_pcb ~sw t params id keepalive in
    (* A hack here because we create the pcb only after the SYN-ACK is rx-ed*)
    STATE.tick ~sw pcb.state (State.Send_syn tx_isn);
    (* Add the PCB to our connection table *)
    Hashtbl.add t.channels id pcb;
    Stats.incr_channel ();
    STATE.tick ~sw pcb.state (State.Recv_synack ack_number);
    (* xmit ACK *)
    TXS.output pcb.txq (Cstruct.create 0);
    pcb

  let is_correct_ack ~tx_isn ~ack_number =
    Sequence.compare (Sequence.succ tx_isn) ack_number = 0

  let process_reset t id ~ack ~ack_number =
    log_with_stats "process-reset" t;
    if ack then
      match hashtbl_find t.connects id with
      | Some (wakener, tx_isn, _) ->
          (* We don't send data in the syn request, so the expected ack is tx_isn + 1 *)
          if is_correct_ack ~tx_isn ~ack_number then (
            Hashtbl.remove t.connects id;
            Stats.decr_connect ();
            Eio.Promise.resolve wakener (Error Tcpip.Tcp.Refused);
            ())
          else ()
      | None -> (
          match hashtbl_find t.listens id with
          | Some (_, (_, pcb)) ->
              Hashtbl.remove t.listens id;
              Stats.decr_listen ();
              Eio.Stream.add t.operations (State_tick {
                state = pcb.state;
                action = State.Recv_rst
              })(*
              TODO 
              Eio.Switch.fail sw (Failure "cancelled") *)
          | None ->
              (* Incoming RST possibly to listen port - ignore per RFC793 pg65 *)
              ())
    else (* rst without ack, drop it *)
      ()

  let process_synack t id ~tx_wnd ~ack_number ~sequence ~options ~syn ~fin =
    log_with_stats "process-synack" t;
    match hashtbl_find t.connects id with
    | Some (wakener, tx_isn, keepalive) ->
        if is_correct_ack ~tx_isn ~ack_number then (
          Hashtbl.remove t.connects id;
          Stats.decr_connect ();
          let rx_wnd = 65535 in
          (* TODO: fix hardcoded value - it assumes that this value was
             sent in the SYN *)
          let rx_wnd_scaleoffer = wscale_default in
          Eio.Stream.add t.operations (New_client_connect {
            pcb_params={ tx_wnd; sequence; options; tx_isn; rx_wnd; rx_wnd_scaleoffer };
            id;
            ack_number;
            keepalive;
            wakener
          }))
        else
          (* Normally sending a RST reply to a random pkt would be in
             order but here we stay quiet since we are actively trying
             to connect this id *)
          ()
    | None ->
        (* Incoming SYN-ACK with no pending connect and no matching pcb
           - send RST *)
        Tx.send_rst t id ~sequence ~ack_number ~syn ~fin
  (* discard errors; we won't retry *)

  let process_syn t id ~tx_wnd ~ack_number ~sequence ~options ~syn ~fin =
    log_with_stats "process-syn" t;
    match Hashtbl.find_opt t.listeners (WIRE.src_port id) with
    | Some (keepalive, process) ->
        let tx_isn = Sequence.of_int32 (Randomconv.int32 Random.generate) in
        (* TODO: make this configurable per listener *)
        let rx_wnd = 65535 in
        let rx_wnd_scaleoffer = wscale_default in

        Eio.Stream.add t.operations (New_server_connect {
          pcb_params={ tx_wnd; sequence; options; tx_isn; rx_wnd; rx_wnd_scaleoffer };
          id;
          keepalive;
          process
        });
    | None ->
        Tx.send_rst t id ~sequence ~ack_number ~syn ~fin
  (* discard errors; we won't retry *)

  (* Blocking read on a PCB *)
  let read pcb =
    match User_buffer.Rx.take_l pcb.urx with
    | None -> Ok `Eof
    | Some t -> Ok (`Data t)

  (* Maximum allowed write *)
  let write_available pcb =
    (* Our effective outgoing MTU is what can fit in a page *)
    min 4000
      (min (Window.tx_mss pcb.wnd) (Int32.to_int (UTX.available pcb.utx)))

  (* Wait for more write space *)
  let write_wait_for pcb sz = UTX.wait_for pcb.utx (Int32.of_int sz)

  let rec writefn pcb wfn data =
    match State.state pcb.state with
    (* but it's only appropriate to send data if the connection is ready for it *)
    | State.Established | State.Close_wait -> (
        let len = Cstruct.length data in
        match write_available pcb with
        | 0 ->
            (* no room at all; we must wait *)
            write_wait_for pcb 1;
            writefn pcb wfn data
        | av_len when av_len >= len ->
            (* we have enough room for the whole packet *)
            wfn [ data ]
        | av_len -> (
            (* partial send is possible *)
            let sendable = Cstruct.sub data 0 av_len in
            writefn pcb wfn sendable;
            writefn pcb wfn @@ Cstruct.sub data av_len (len - av_len)))
    | _ -> raise Not_ready

  (* Blocking write on a PCB *)
  let write pcb data = writefn pcb (UTX.write pcb.utx) data

  let writev pcb data = List.iter (write pcb) data

  (* Close - no more will be written *)
  let close pcb = Tx.close pcb
  let chunk_cs = Cstruct.create 10000

  let fallback_copy src flow = 
    try
      while true do
        let got = Eio.Flow.read src chunk_cs in
        write flow (Cstruct.sub chunk_cs 0 got)
      done
    with End_of_file -> ()
  
  let copy_with_rsb rsb flow =
    try
      rsb @@ fun cstruct ->
      writev flow cstruct;
      Cstruct.lenv cstruct
    with
    | End_of_file -> ()

  class flow_obj t (flow : connection) =
    object (self: < Eio.Flow.source; Eio.Flow.sink; dst; .. > )
      method probe _ = None

      method copy (src : #Eio.Flow.source) =
        let rec aux = function
          | Eio.Flow.Read_source_buffer rsb :: _ -> copy_with_rsb rsb flow
          | _ :: xs -> aux xs
          | [] -> fallback_copy src flow
        in
        aux (Eio.Flow.read_methods src)

      method private read_source_buffer fn =
        match read flow with
        | Ok (`Data buffer) -> 
          let rec loop = function
            | [] -> ()
            | buf ->
              let sz = fn buf in
              loop (Cstruct.shiftv buf sz)
          in
          loop [buffer]
        | Ok `Eof -> raise End_of_file
        | Error _ -> raise End_of_file
      
      method read_into buf =
        match read flow with
        | Ok (`Data buffer) ->
            Cstruct.blit buffer 0 buf 0 (Cstruct.length buffer);
            Cstruct.length buffer
        | Ok `Eof -> raise End_of_file
        | Error _ -> raise End_of_file

      method read_methods = [
        Eio.Flow.Read_source_buffer self#read_source_buffer
      ]

      method shutdown (_ : [ `All | `Receive | `Send ]) = close t flow
      method close = close t flow
      method dst = WIRE.dst flow.id, WIRE.dst_port flow.id
    end

  (* INPUT *)

  let process_ack t id ~pkt =
    let open RXS in
    log_with_stats "process-ack" t;
    match hashtbl_find t.listens id with
    | Some (tx_isn, (pushf, newconn)) ->
        if Tcp_packet.(is_correct_ack ~tx_isn ~ack_number:pkt.header.ack_number)
        then (
          (* Established connection - promote to active channels *)
          Hashtbl.remove t.listens id;
          Stats.decr_listen ();
          Hashtbl.add t.channels id newconn;
          Stats.incr_channel ();
          (* Finish processing ACK, so pcb.state is correct *)
          Rx.input t pkt newconn;
          (* send new connection up to listener *)

          Eio.Stream.add t.operations (New_connection {pushf; newconn}))
        else (* No RST because we are trying to connect on this id *)
          ()
    | None -> (
        match hashtbl_find t.connects id with
        | Some _ ->
            (* No RST because we are trying to connect on this id *)
            ()
        | None ->
            let { sequence; Tcp_packet.ack_number; syn; fin; _ } = pkt.header in
            (* ACK but no matching pcb and no listen - send RST *)
            Tx.send_rst t id ~sequence ~ack_number ~syn ~fin |> fun _ ->
            () (* if send fails, who cares *))

  let input_no_pcb t (parsed, payload) id =
    if not t.active then (* TODO: eventually send an RST? *)
      ()
    else
      let {
        sequence;
        Tcp_packet.ack_number;
        window;
        options;
        syn;
        fin;
        rst;
        ack;
        _;
      } =
        parsed
      in
      match (rst, syn, ack) with
      | true, _, _ -> process_reset t id ~ack ~ack_number
      | false, true, true ->
          process_synack t id ~ack_number ~sequence ~tx_wnd:window ~options ~syn
            ~fin
      | false, true, false ->
          process_syn t id ~tx_wnd:window ~ack_number ~sequence ~options ~syn
            ~fin
      | false, false, true ->
          let open RXS in
          process_ack t id ~pkt:{ header = parsed; payload }
      | false, false, false ->
          Log.info (fun f ->
              f
                "incoming packet matches no connection table entry and has no \
                 useful flags set; dropping it");
          ()

  (* Main input function for TCP packets *)
  let input t ~src ~dst data =
    let open Tcp_packet in
    match Unmarshal.of_cstruct data with
    | Error s -> Log.info (fun f -> f "parsing TCP header failed: %s" s)
    | Ok (pkt, payload) ->
        let id =
          WIRE.v ~src_port:pkt.dst_port ~dst_port:pkt.src_port ~dst:src ~src:dst
        in
        (* Lookup connection from the active PCB hash *)
        with_hashtbl t.channels id
          (* PCB exists, so continue the connection state machine in tcp_input *)
          (Rx.input t RXS.{ header = pkt; payload })
          (* No existing PCB, so check if it is a SYN for a listening function *)
          (input_no_pcb t (pkt, payload))

  let getid t dst dst_port =
    (* TODO: make this more robust and recognise when all ports are gone *)
    let islistener _t _port =
      (* TODO keep a list of active listen ports *)
      false
    in
    let idinuse t id =
      Hashtbl.mem t.channels id || Hashtbl.mem t.connects id
      || Hashtbl.mem t.listens id
    in
    let inuse t id = islistener t (WIRE.src_port id) || idinuse t id in
    let rec bumpport t =
      (match t.localport with
      | 65535 -> t.localport <- 10000
      | _ -> t.localport <- t.localport + 1);
      let id =
        WIRE.v ~src:(Ip.src t.ip ~dst) ~src_port:t.localport ~dst ~dst_port
      in
      if inuse t id then bumpport t else id
    in
    bumpport t

  (* SYN retransmission timer *)
  let rec connecttimer t id tx_isn options window count =
    let rxtime =
      match count with 0 -> 3 | 1 -> 6 | 2 -> 12 | 3 -> 24 | _ -> 48
    in
    Eio.Time.sleep t.clock (Int.to_float rxtime);
    match hashtbl_find t.connects id with
    | None -> ()
    | Some (wakener, isn, _) ->
        if isn = tx_isn then
          if count > 3 then (
            Hashtbl.remove t.connects id;
            Stats.decr_connect ();
            Eio.Promise.resolve wakener (Error Tcpip.Tcp.Timeout))
          else
            match Tx.send_syn t id ~tx_isn ~options ~window with
            | () -> connecttimer t id tx_isn options window (count + 1)
            | exception Tcpip.Ip.No_route _s ->
                (* normal mechanism for recovery is fine *)
                connecttimer t id tx_isn options window (count + 1)
            | exception Tcpip.Ip.Would_fragment ->
                (* this should not happen, if we've a transport that fragments syn.. *)
                Log.err (fun m ->
                    m "syn retransmission timer returned would fragment")
            | exception e -> Eio.Promise.resolve wakener (Error e)
        else ()

  let connect ?keepalive t ~dst ~dst_port =
    let id = getid t dst dst_port in
    let tx_isn = Sequence.of_int32 (Randomconv.int32 Random.generate) in
    (* TODO: This is hardcoded for now - make it configurable *)
    let rx_wnd_scaleoffer = wscale_default in
    let options =
      [
        Options.MSS (Ip.mtu t.ip ~dst - Tcp_wire.sizeof_tcp);
        Options.Window_size_shift rx_wnd_scaleoffer;
      ]
    in
    let window = 5840 in
    let th, wakener = Eio.Promise.create ~label:"TCP connect" () in
    if Hashtbl.mem t.connects id then (
      Log.info (fun f ->
          f
            "duplicate attempt to make a connection: [%a]. Removing the old \
             state and replacing with new attempt"
            WIRE.pp id);
      Hashtbl.remove t.connects id;
      Stats.decr_connect ());
    Hashtbl.add t.connects id (wakener, tx_isn, keepalive);
    Stats.incr_connect ();
    Tx.send_syn t id ~tx_isn ~options ~window;
     (* keep trying *)
    Eio.Stream.add t.operations (Connect_timer {id; tx_isn; options; window});
    th

  let log_failure daddr dport exn =
    match exn with
    | Tcpip.Tcp.Timeout ->
        Log.info (fun fmt ->
            fmt "Timeout attempting to connect to %a:%d\n%!" Ip.pp_ipaddr daddr
              dport)
    | Tcpip.Tcp.Refused ->
        Log.info (fun fmt ->
            fmt "Refused connection to %a:%d\n%!" Ip.pp_ipaddr daddr dport)
    | e ->
        Log.info (fun fmt ->
            fmt "%s error connecting to %a:%d\n%!" (Printexc.to_string e) Ip.pp_ipaddr daddr
              dport)

  let create_connection ?keepalive tcp (daddr, dport) =
    if not tcp.active then raise Not_ready (* TODO: custom error variant *)
    else
      match
        Eio.Promise.await (connect ?keepalive tcp ~dst:daddr ~dst_port:dport)
      with
      | Error e ->
          log_failure daddr dport e;
          raise e
      | Ok conn -> new flow_obj tcp conn

  let rec handle t ~sw operations =
    let v = Eio.Stream.take operations in
    Eio.Fiber.fork ~sw (fun () -> 
      Eio.Private.Ctf.label "tcp.handle";
      match v with
      | New_client_connect {
          pcb_params;
          id;
          ack_number;
          keepalive;
          wakener
        } ->
        let pcb =
          new_client_connection ~sw t
            pcb_params
            id ack_number keepalive
        in
        Eio.Promise.resolve wakener (Ok pcb)
      | New_server_connect {
          pcb_params;
          id;
          process;
          keepalive
        } ->
        new_server_connection ~sw t pcb_params id process keepalive
        |> fun (_ : flow) -> ()
      | New_connection {
          pushf;
          newconn
        } -> pushf (new flow_obj t newconn)
      | Connect_timer {
          id;
          tx_isn;
          options;
          window  
        } -> connecttimer t id tx_isn options window 0
      | State_tick {
          state;
          action;
        } -> 
          STATE.tick ~sw state action
      | Keepalive_refresh {
        keepalive
        } -> 
          KEEPALIVE.refresh ~sw keepalive
      | RXS_input {
          rxq; segment
        } -> 
          RXS.input ~sw rxq segment
    );
    handle t ~sw operations
  
  (* Construct the main TCP thread *)
  let connect ~sw ~clock ip =
    let localport =
      1024 + Randomconv.int ~bound:(0xFFFF - 1024) Random.generate
    in
    let listens = Hashtbl.create 1 in
    let connects = Hashtbl.create 1 in
    let channels = Hashtbl.create 7 in
    let operations = Eio.Stream.create max_int in
    let t = {
      ip;
      operations;
      clock;
      listeners = Hashtbl.create 7;
      active = true;
      localport;
      channels;
      listens;
      connects;
    }
    in
    Eio.Fiber.fork ~sw (fun () -> 
      Eio.Private.Ctf.label "tcp.handle.init";
      handle t ~sw operations);
    t
    

  let disconnect t =
    t.active <- false;
    let conns =
      Hashtbl.fold (fun _ pcb acc -> pcb :: acc) t.channels []
    in
    Fiber.all (List.map (fun pcb () -> close t pcb) conns);
    Hashtbl.reset t.listens;
    Hashtbl.reset t.connects
  (* TODO: should there be Lwt tasks being cancelled? *)
end
