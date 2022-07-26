(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)


module Reader = Parse.Reader
module Writer = Serialize.Writer

open Eio.Std

type request_handler = Reqd.t -> unit

type error =
  [ `Bad_gateway | `Bad_request | `Internal_server_error | `Exn of exn]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> [`write] Body.t) -> unit

let default_error_handler ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
  in
  let body = handle Headers.empty in
  Body.write_string body message;
  Body.close_writer body
;;

let requests ~sw ~make_reqd handler =
  let open Angstrom in
  fix @@ fun requests ->
  Parse.at_end_of_input >>= function
  | true -> Angstrom.return (Ok ())
  | false ->
    Parse.request <* commit >>= fun request ->
    let k reqd =
      Reqd.flush_response_body reqd;
      Writer.wakeup reqd.writer;
      assert (Reqd.is_complete reqd);
      if Reqd.persistent_connection reqd then (
        requests
      ) else (
        Angstrom.return (Ok ())
      )
    in
    match Request.body_length request with
    | `Error `Bad_request -> return (Error (`Bad_request request))
    | `Fixed 0L  ->
      let reqd = make_reqd request Body.empty in
      handler reqd;
      k reqd
    | `Fixed _ | `Chunked | `Close_delimited as encoding ->
      let request_body = Body.create_reader Bigstringaf.empty in
      let reqd = make_reqd request request_body in
      let request_done = Fibre.fork_promise ~sw (fun () -> handler reqd) in
      Parse.body ~encoding request_body >>= fun () ->
      Promise.await_exn request_done;
      k reqd
;;

let handle ?(config=Config.default) ?(error_handler=default_error_handler)
    ~sw
    ~(read:int -> Angstrom.bigstring * int * int * Reader.AU.more)
    ~write
    handler =
  let
    { Config
    . response_buffer_size
    ; response_body_buffer_size
    ; _ } = config
  in
  let writer = Writer.create ~buffer_size:response_buffer_size () in
  let response_body_buffer = Bigstringaf.create response_body_buffer_size in
  let make_reqd request request_body =
    Reqd.create error_handler request request_body writer response_body_buffer
  in
  Fibre.fork ~sw (fun () -> Writer.run ~write writer);
  let x = Angstrom.Unbuffered.parse ~read (requests ~sw ~make_reqd handler) in
  begin match Angstrom.Unbuffered.state_to_result x with
    | Ok (Ok ()) -> ()
    | Ok (Error (`Bad_request request)) ->
      error_handler ~request `Bad_request (fun headers ->
          Writer.write_response writer (Response.create ~headers `Bad_request);
          Body.writer_of_faraday (Writer.faraday writer)
            ~when_ready_to_write:(fun () -> Writer.wakeup writer))
    | Error msg ->
      print_endline msg;
      error_handler `Bad_request (fun headers ->
          Writer.write_response writer (Response.create ~headers `Bad_request);
          Body.writer_of_faraday (Writer.faraday writer)
            ~when_ready_to_write:(fun () -> Writer.wakeup writer))
  end;
  Writer.close writer;
  Writer.wakeup writer
