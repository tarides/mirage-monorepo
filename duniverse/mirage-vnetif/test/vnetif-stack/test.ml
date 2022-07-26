(*
 * Copyright (c) 2015-20 Magnus Skjegstad <magnus@skjegstad.com>
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

module Stack (B : Vnetif.BACKEND) = struct
  module V = Vnetif_stack.Vnetif_stack (B) (Mirage_random_test) (Mclock)
  include V
end

let buffer = Cstruct.create 1024

let connect_test env () =
  let module Backend = Basic_backend.Make in
  let module Stack = Stack (Backend) in
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let backend = Backend.create ~use_async_readers:sw () in

  let test_msg = "This is a connect test. ABCDEFGHIJKLMNOPQRSTUVWXYZ" in


  let accept client_l flow expected =
    match Eio.Flow.read flow buffer with
    | exception End_of_file -> Alcotest.failf "eof while reading from socket"
    | len ->
        let recv_str = Cstruct.to_string ~len buffer in
        Alcotest.(check string)
          "server and client strings matched" expected recv_str;
        Eio.Mutex.unlock client_l
  in

  let client_cidr = Ipaddr.V4.Prefix.of_string_exn "10.0.0.10/24" in
  let server_cidr = Ipaddr.V4.Prefix.of_string_exn "10.0.0.11/24" in

  let timeout_in_s = 1. in

  (* mutex to signal success from server to client *)
  let accept_l = Eio.Mutex.create () in
  (* mutex to signal client that server is listening *)
  let listen_l = Eio.Mutex.create () in

  Eio.Mutex.with_lock accept_l (fun _ ->
      Eio.Mutex.with_lock listen_l (fun _ ->
          Eio.Fiber.any
            [
              (* Cancellation timer *)
              (fun () ->
                Eio.Time.sleep clock timeout_in_s;
                Eio.Switch.fail sw (Failure "timeout");
                Alcotest.failf "timeout: test timed out after %f seconds"
                  timeout_in_s);
              (* Server side *)
              (fun () ->
                Eio.Switch.run @@ fun sw ->
                let s1 =
                  Stack.create_stack_ipv4 ~sw ~clock ~cidr:server_cidr
                    ~unlock_on_listen:listen_l backend
                in
                Stack.V4.TCPV4.listen (Stack.V4.tcpv4 s1) ~port:80 (fun f ->
                    accept accept_l f test_msg);
                Stack.V4.listen s1;
                Alcotest.failf "server: listen should never exit");
              (* Client side *)
              (fun () ->
                try
                  Eio.Switch.run @@ fun sw ->
                  Eio.Mutex.lock listen_l;
                  Eio.Mutex.lock listen_l; (* Netif.listen is called twice, so the mutex is unlocked twice. *)
                  (* wait for server to unlock with call to listen *)
                  let s2 =
                    Stack.create_stack_ipv4 ~sw ~clock ~cidr:client_cidr backend
                  in
                  let flow = Stack.V4.TCPV4.create_connection (Stack.V4.tcpv4 s2)
                      (Ipaddr.V4.Prefix.address server_cidr, 80)
                  in
                  (try
                     Eio.Flow.copy
                       (Eio.Flow.cstruct_source [ Cstruct.of_string test_msg ])
                       flow
                   with End_of_file -> Alcotest.failf "write: end_of_file");
                  Eio.Flow.shutdown flow `All;
                  Eio.Mutex.lock accept_l;
                  (* wait for accept to unlock *)
                  Eio.Switch.fail sw Not_found
                with Not_found -> ());
            ]));
    Backend.disconnect backend

let () =
  let rand_seed = 0 in
  Random.init rand_seed;
  Printf.printf "Testing with rand_seed %d\n" rand_seed;
  (*Mirage_random_test.initialize();*)
  Eio_unix.Ctf.with_tracing "trace.ctf" @@ fun () ->
  Eio_linux.run @@ fun env ->
  Alcotest.run "mirage-vnetif"
    [ ("stack.v4", [ Alcotest.test_case "connect" `Quick (connect_test env) ]) ]
