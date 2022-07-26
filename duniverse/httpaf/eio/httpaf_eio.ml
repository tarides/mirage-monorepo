(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.
    Copyright (c) 2018 Anton Bachin

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

open Eio.Std

module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Cstruct.t -> int) -> int
  val commit : t -> int -> unit
  val peek : t -> Cstruct.t
  val read : t -> #Eio.Flow.source -> int
end = struct
  type t =
    { buffer      : Cstruct.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let buffer = Cstruct.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0
    then begin
      Cstruct.blit t.buffer t.off t.buffer 0 t.len;
      t.off <- 0;
    end

  let get t ~f =
    let n = f (Cstruct.sub t.buffer t.off t.len) in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0;
    n

  let peek t =
    Cstruct.sub t.buffer t.off t.len

  let commit t n =
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0

  let read t flow =
    compress t;
    let n = Eio.Flow.read flow (Cstruct.shift t.buffer t.len) in
    t.len <- t.len + n;
    n
end

let read flow buffer =
  match Buffer.read buffer flow with
  | got -> `Ok got
  | exception End_of_file
  | exception Eio.Net.Connection_reset _ -> `Eof

let cstruct_of_faraday { Faraday.buffer; off; len } = Cstruct.of_bigarray ~off ~len buffer

let write flow iovecs =
  let data = List.map cstruct_of_faraday iovecs in
  Eio.Flow.copy (Eio.Flow.cstruct_source data) flow

module Config = Httpaf.Config

module Server = struct
  let create_connection_handler ?(config=Config.default) ~error_handler request_handler =
    fun ~sw (socket : #Eio.Flow.two_way) client_addr ->
      let module Server_connection = Httpaf.Server_connection in
      let request_handler = request_handler client_addr in
      let error_handler = error_handler client_addr in
      let read_buffer = Buffer.create config.read_buffer_size in
      let read committed =
        Buffer.commit read_buffer committed;
        let more =
          match Buffer.read read_buffer socket with
          | exception End_of_file
          | exception Eio.Net.Connection_reset _ -> Angstrom.Unbuffered.Complete
          | _ -> Angstrom.Unbuffered.Incomplete
        in
        let { Cstruct.buffer; off; len } = Buffer.peek read_buffer in
        buffer, off, len, more
      in
      let write io_vectors =
        match write socket io_vectors with
        | () -> `Ok (List.fold_left (fun acc f -> acc + f.Faraday.len) 0 io_vectors)
        | exception Unix.Unix_error (Unix.EPIPE, _, _) -> `Closed
      in
      Server_connection.handle ~sw ~error_handler ~read ~write request_handler
end

module Client = struct
  let shutdown socket cmd =
    try Eio.Flow.shutdown socket cmd
    with Unix.Unix_error(Unix.ENOTCONN, _, _) -> ()

  let request ?(config=Config.default) ~sw socket request ~error_handler ~response_handler =
    let module Client_connection = Httpaf.Client_connection in
    let request_body, connection =
      Client_connection.request ~config request ~error_handler ~response_handler in
    let read_buffer = Buffer.create config.read_buffer_size in
    let rec read_loop () =
      match Client_connection.next_read_operation connection with
      | `Read ->
        begin match read socket read_buffer with
          | `Eof ->
            let _ : int = Buffer.get read_buffer ~f:(fun { Cstruct.buffer = bigstring; off; len } ->
                Client_connection.read_eof connection bigstring ~off ~len) in
            read_loop ()
          | `Ok _ ->
            let _ : int = Buffer.get read_buffer ~f:(fun { Cstruct.buffer = bigstring; off; len } ->
                Client_connection.read connection bigstring ~off ~len) in
            read_loop ()
        end
      | `Close ->
        shutdown socket `Receive;
        raise Exit
    in
    let rec write_loop () =
      match Client_connection.next_write_operation connection with
      | `Write io_vectors ->
        let result =
          match write socket io_vectors with
          | () -> `Ok (List.fold_left (fun acc f -> acc + f.Faraday.len) 0 io_vectors)
          | exception Unix.Unix_error (Unix.EPIPE, _, _) -> `Closed
        in
        Client_connection.report_write_result connection result;
        write_loop ()
      | `Yield ->
        let pause, resume = Promise.create () in
        Client_connection.yield_writer connection (fun () -> Promise.resolve resume ());
        Promise.await pause;
        write_loop ()
      | `Close _ ->
        shutdown socket `Send;
        raise Exit
    in
    Fibre.fork ~sw
      (fun () ->
         try
           read_loop ()
         with
         | Exit -> Logs.info (fun f -> f "Read loop done")
         | ex ->
           Logs.warn (fun f -> f "Error reading from connection: %a" Fmt.exn ex);
           Client_connection.report_exn connection ex
      );
    Fibre.fork ~sw
      (fun () ->
         try
           write_loop ()
         with
         | Exit -> Logs.info (fun f -> f "Write loop done")
         | ex ->
           Logs.warn (fun f -> f "Error writing to connection: %a" Fmt.exn ex);
           Client_connection.report_exn connection ex
      );
    request_body
end
