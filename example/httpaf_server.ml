open Httpaf
open Eio.Std

let text = "Hello world!"

let text = Bigstringaf.of_string ~off:0 ~len:(String.length text) text

let headers = Headers.of_list ["content-length", string_of_int (Bigstringaf.length text)]
let request_handler _ reqd =
  let request = Reqd.request reqd in
  match request.target with
  | "/" ->
    let response_ok = Response.create ~headers `OK in
    Reqd.respond_with_bigstring reqd response_ok text
  | "/exit" ->
    exit 0
  | _   ->
    let msg = "Route not found" in
    let headers = Headers.of_list ["content-length", string_of_int (String.length msg)] in
    let response_nf = Response.create ~headers `Not_found in
    Reqd.respond_with_string reqd response_nf msg

let error_handler _ ?request:_ error start_response =
  let response_body = start_response Httpaf.Headers.empty in
  begin match error with
  | `Exn exn ->
    Httpaf.Body.write_string response_body (Printexc.to_string exn);
    Httpaf.Body.write_string response_body "\n";
  | #Httpaf.Status.standard as error ->
    Httpaf.Body.write_string response_body (Httpaf.Status.default_reason_phrase error)
  end;
  Httpaf.Body.close_writer response_body

let log_connection_error ex =
  traceln "Uncaught exception handling client: %a" Fmt.exn ex

let run_domain ssock =
  traceln "Running server in domain %d" (Domain.self () :> int);
  Switch.run @@ fun sw ->
  let handle_connection = Httpaf_eio.Server.create_connection_handler request_handler ~error_handler
  in
  (* Wait for clients, and fork off echo servers. *)
  while true do
    Eio.Net.accept_fork ssock ~sw ~on_error:log_connection_error 
      (fun flow str -> handle_connection flow str)
  done

let main ~net ~domain_mgr ~n_domains port backlog =
  Switch.run @@ fun sw ->
  let ssock = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog @@ `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  traceln "Echo server listening on 127.0.0.1:%d" port;
  traceln "Starting %d domains..." n_domains;
  for _ = 2 to n_domains do
    Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr
          (fun () ->
             (* Note: really we should dup [ssock] for each domain,
                but [run_domain] won't close it anyway. *)
             run_domain ssock
          )
      )
  done;
  run_domain ssock

let () = 
(*
  Logs.(set_level (Some Debug));
  Logs.set_reporter (Logs_fmt.reporter ());
*)
(*
  let buffer = Ctf.Unix.mmap_buffer ~size:0x100000 "trace/trace.ctf" in
  let trace_config = Ctf.Control.make buffer in
  Ctf.Control.start trace_config;
*)
  (* Eio_luv.run @@ fun env -> *)
  Eio_main.run @@ fun env ->
  let n_domains =
    match Sys.getenv_opt "HTTPAF_EIO_DOMAINS" with
    | Some d -> int_of_string d
    | None -> 1
  in
  main 8080 128
    ~net:(Eio.Stdenv.net env)
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~n_domains