(*
 * Copyright (c) 2012 Balraj Singh <bs375@cl.cam.ac.uk>
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

module Lwt = struct end

let src = Logs.Src.create "tcptimer" ~doc:"Mirage TCP Tcptimer module"
module Log = (val Logs.src_log src : Logs.LOG)

type time = int64

type tr =
  | Stoptimer
  | Continue of Sequence.t
  | ContinueSetPeriod of (time * Sequence.t)

type t = {
  expire: (Sequence.t -> tr);
  mutable running: bool;
  notify: (time option * Sequence.t) Eio.Stream.t;
  clock: Eio.Time.clock;
}

let timerloop t seq period_ns =
  Log.debug (fun f -> f "timerloop");
  Stats.incr_timer ();
  let rec aux t s period_ns =
    Log.debug (fun f -> f "timerloop: sleeping for %Lu ns" period_ns);
    Eio.Time.sleep t.clock (Int64.to_float period_ns /. 1_000_000_000.);
    match t.expire s with
    | Stoptimer ->
      Stats.decr_timer ();
      Log.debug (fun f -> f "timerloop: stoptimer");
      ()
    | Continue d ->
      Log.debug (fun f -> f "timerloop: continuer");
      aux t d period_ns
    | ContinueSetPeriod (p, d) ->
      Log.debug (fun f -> f "timerloop: continuesetperiod (new period: %Lu ns)" p);
      aux t d p
  in
  aux t seq period_ns

let listener_thread v period_ns () =
  let rec loop period_ns =
    let (next_period_ns, seq) = Eio.Stream.take v.notify in 
    v.running <- true;
    let next_period_ns = 
      Option.value next_period_ns ~default:period_ns 
    in
    timerloop v seq next_period_ns;
    v.running <- false;
    loop next_period_ns
  in
  loop period_ns

let t ~sw ~period_ns ~expire ~clock =
  let notify = Eio.Stream.create 1 in
  let v = {notify; expire; running = false; clock} in 
  Eio.Fiber.fork ~sw (listener_thread v period_ns);
  v
  
let restart t ?p sequence =
  if not t.running then begin
    Eio.Stream.add t.notify (p, sequence)
  end else
    ()
