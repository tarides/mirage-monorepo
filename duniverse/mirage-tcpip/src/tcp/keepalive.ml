(*
 * Copyright (c) 2017 Docker Inc
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

type action = [
  | `SendProbe
  | `Wait of Duration.t
  | `Close
]

type state = {
  probes_sent: int
}

let alive = {
  probes_sent = 0;
}

let next ~configuration ~ns state =
  let open Tcpip.Tcp.Keepalive in
  let after_ns = configuration.after in
  (* Wait until [time] has gone past *)
  if after_ns > ns
  then `Wait (Int64.sub after_ns ns), alive
  else begin
    let sending_probes_for_ns = Int64.sub ns after_ns in
    let interval_ns = configuration.interval in
    let should_have_sent = Int64.(to_int (div sending_probes_for_ns interval_ns)) in
    if should_have_sent > configuration.probes
    then `Close, state
    else
      if should_have_sent > state.probes_sent
      then `SendProbe, { probes_sent = should_have_sent } (* we don't want to send back-to-back probes *)
      else begin
        let since_last_probe_ns = Int64.rem sending_probes_for_ns interval_ns in
        `Wait (Int64.sub interval_ns since_last_probe_ns), state
      end
  end

  module Make(Clock:Mirage_clock.MCLOCK) = struct
    type t = {
      configuration: Tcpip.Tcp.Keepalive.t;
      callback: ([ `SendProbe | `Close ] -> unit);
      clock: Eio.Time.clock;
      mutable state: state;
      mutable cancel: unit Eio.Promise.u;
      mutable start: int64;
    }
    (** A keep-alive timer *)

    let rec restart t =
      let ns = Int64.sub (Clock.elapsed_ns ()) t.start in
      match next ~configuration:t.configuration ~ns t.state with
      | `Wait ns, state ->
        Eio.Time.sleep t.clock (Int64.to_float ns /. 1_000_000_000.);
        t.state <- state;
        restart t
      | `SendProbe, state ->
        t.callback `SendProbe;
        t.state <- state;
        restart t
      | `Close, _ ->
        t.callback `Close;
        ()

    let create ~sw ~clock configuration callback =
      let state = alive in
      let start = Clock.elapsed_ns () in
      let promise_cancel, cancel = Eio.Promise.create ~label:"keepalive" () in
      let t = { configuration; callback; state; cancel; clock; start } in
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Private.Ctf.label "tcp.keepalive.create";
        Eio.Fiber.any [
          (fun () -> Eio.Promise.await promise_cancel);
          (fun () -> restart t)
        ]);
      t

    let refresh ~sw t =
      t.start <- Clock.elapsed_ns ();
      t.state <- alive;
      let promise_cancel, cancel = Eio.Promise.create ~label:"keepalive" () in
      t.cancel <- cancel;
      Eio.Promise.resolve t.cancel ();
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Private.Ctf.label "tcp.keepalive.refresh";
        Eio.Fiber.any [
          (fun () -> Eio.Promise.await promise_cancel);
          (fun () -> restart t)
        ])
  end
