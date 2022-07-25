module Lwt = struct end

let failf fmt = Fmt.kstr (fun s -> Alcotest.fail s) fmt
let ( let* ) = Result.bind

let or_error name fn t =
  match fn t with Error _ -> failf "or_error starting %s" name | Ok t -> t

let expect_exception error name fn t =
  match fn t with
  | exception exn when exn = error -> t
  | _ -> failf "expected error on %s" name

let ipv4_packet = Alcotest.testable Ipv4_packet.pp Ipv4_packet.equal
let udp_packet = Alcotest.testable Udp_packet.pp Udp_packet.equal
let tcp_packet = Alcotest.testable Tcp.Tcp_packet.pp Tcp.Tcp_packet.equal
let cstruct = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal

let sequence =
  let eq x y = Tcp.Sequence.compare x y = 0 in
  Alcotest.testable Tcp.Sequence.pp eq

let options = Alcotest.testable Tcp.Options.pp Tcp.Options.equal

let fast_clock ?(time_reduction_factor = 100.) clock: Eio.Time.clock =
  let zero = Eio.Time.now clock in
  object
    method now = (clock#now -. zero) *. time_reduction_factor

    method sleep_until f = clock#sleep_until (zero +. (f /. time_reduction_factor))
  end

let switch_run_cancel_on_return (type a) fn =
  let exception Stopped of a in
  try
    Eio.Switch.run @@ fun sw ->
    let res = fn sw in
    Eio.Switch.fail sw (Stopped res);
    res (* dead code*)
    
  with
  | Stopped res -> res
  | Eio.Exn.Multiple exns ->
    match 
      List.find_map (function
        | Stopped v -> Some v
        | _ -> None) exns,
      exns
    with
    | (Some result), _ -> result
    | None, [] -> assert false
    | None, [exn] ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt
    | None, exns -> 
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Eio.Exn.Multiple exns) bt

let run program () =
  Eio_linux.run @@ fun env ->
  switch_run_cancel_on_return @@ fun sw ->
  program ~sw ~env ()
