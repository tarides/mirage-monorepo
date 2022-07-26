module Arg = Caml.Arg

open Httpaf
open Httpaf_eio
open Fibreslib

let read_all ch =
  let buffer = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let rec aux () =
    match input ch b 0 (Bytes.length b) with
    | 0 -> Buffer.contents buffer
    | got ->
      Buffer.add_subbytes buffer b 0 got;
      aux ()
  in
  aux ()
    
let main ~network port host =
  print_endline "enter data";
  let body = read_all stdin in
  print_endline "got body";
  Switch.top @@ fun sw ->
  let addresses = Unix.getaddrinfo host (Int.to_string port) [Unix.(AI_FAMILY PF_INET)] in
  let socket = Eio.Network.connect network (List.hd addresses).Unix.ai_addr in
  let finished, notify_finished = Promise.create () in
  let response_handler =
    Httpaf_examples.Client.print ~on_eof:(Promise.fulfill notify_finished)
  in
  let headers =
    Headers.of_list
    [ "content-length"   , (Int.to_string (String.length body))
    ; "connection"       , "close"
    ; "host"             , host
    ]
  in
  let request_body =
    Client.request
      ~sw
      ~error_handler:Httpaf_examples.Client.error_handler
      ~response_handler
      socket
      (Request.create ~headers `POST "/")
  in
  Body.write_string request_body body;
  Body.close_writer request_body;
  Promise.await finished

let () =
  let host = ref None in
  let port = ref 8080 in

  Arg.parse
    ["-p", Set_int port, " Port number (8080 by default)"]
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
