module Arg = Caml.Arg

open Fibreslib
open Httpaf
open Httpaf_eio

let main ~network port host =
  Switch.top @@ fun sw ->
  let addresses = Unix.getaddrinfo host (Int.to_string port) [Unix.(AI_FAMILY PF_INET)] in
  let socket = Eio.Network.connect network (List.hd addresses).Unix.ai_addr in
  let finished, notify_finished = Promise.create () in
  let response_handler =
    Httpaf_examples.Client.print ~on_eof:(Promise.fulfill notify_finished)
  in
  let headers = Headers.of_list [ "host", host ] in
  let request_body =
    Client.request
      ~sw
      ~error_handler:Httpaf_examples.Client.error_handler
      ~response_handler
      socket
      (Request.create ~headers `GET "/")
  in
  Body.close_writer request_body;
  Promise.await finished

let () =
  let host = ref None in
  let port = ref 80 in
  Arg.parse
    ["-p", Set_int port, " Port number (80 by default)"]
    (fun host_argument -> host := Some host_argument)
    "eio_get.exe [-p N] HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  Eunix.run @@ fun env ->
  main !port host
    ~network:(Eio.Stdenv.network env)
