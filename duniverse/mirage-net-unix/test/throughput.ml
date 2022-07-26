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

let sz = 250_000_000
let n = 32

let mtu = Some 64

let test_write ~sw () =
  let t = Netif.connect ~sw "tap0" in
  let mtu = 
    Option.value mtu ~default:(Netif.mtu t) 
  in
  let t0 = Unix.gettimeofday () in

  let data = Cstruct.create_unsafe mtu in
  for i = 0 to ((mtu - 1) / 8) do 
    Cstruct.LE.set_uint64 data (2*i) (Int64.of_int i)
  done;

  Eio.Std.Fiber.all
    (List.init n (fun _ () ->
         for _ = 0 to sz / n / mtu do
           Netif.writev t [data]
         done));
  let t = Unix.gettimeofday () -. t0 in
  Printf.printf "Wrote 250M in %.2fs\n" t;
  Printf.printf "(%.0f pps)\n" (Float.of_int (sz / mtu) /. t)

let tracing = false

let may_trace fn = 
  if tracing then
    Eio_unix.Ctf.with_tracing "trace.ctf" fn
  else
    fn ()

let _ = 
  may_trace @@ fun () ->
  Eio_linux.run @@ fun _ ->
  Eio.Std.Switch.run @@ fun sw -> test_write ~sw ()
