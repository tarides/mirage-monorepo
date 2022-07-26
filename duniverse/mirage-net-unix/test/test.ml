(*
 * Copyright (C) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf

let run test =
  Eio_linux.run @@ fun _ ->
  Eio.Std.Switch.run @@ fun sw -> test ~sw ()

let test_open_close ~sw () =
  let t = Netif.connect ~sw "tap0" in
  printf "tap0: connected\n%!";
  Netif.disconnect t;
  printf "tap0: disconnected\n%!"

let test_write ~sw () =
  let t = Netif.connect ~sw "tap0" in
  let mtu = Netif.mtu t in
  let data = Cstruct.create (mtu+22) in
  Netif.writev t [Cstruct.sub data 0 mtu];
  Netif.writev t [Cstruct.sub data 0 (mtu + 14)];
  Netif.writev t [Cstruct.sub data 0 (mtu + 22)]

let suite =
  [
   (* ("connect", `Quick, fun () -> run test_open_close);*)
    ("write", `Quick, fun () -> run test_write);
  ]

let _ = Alcotest.run "mirage-net-unix" [ ("tests", suite) ]
