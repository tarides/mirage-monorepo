(*
 * Copyright (c) 2015-16 Magnus Skjegstad <magnus@skjegstad.com>
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

module type Backend = sig
  include Vnetif.BACKEND

  val create : sw:Eio.Switch.t -> clock:Eio.Time.clock -> unit -> t
end

(** This backend enforces an Ethernet frame size. *)
module Frame_size_enforced = struct
  module X = Basic_backend.Make

  type t = { xt : X.t; mutable frame_size : int }
  type macaddr = X.macaddr
  type buffer = X.buffer
  type id = X.id

  let register t = X.register t.xt
  let unregister t id = X.unregister t.xt id
  let mac t id = X.mac t.xt id
  let set_listen_fn t id buf = X.set_listen_fn t.xt id buf
  let unregister_and_flush t id = X.unregister_and_flush t.xt id

  let writev t id iovec =
    let len = Cstruct.lenv iovec in
    if len > t.frame_size then raise (Mirage_net.Net.Invalid_length len)
    else X.writev t.xt id iovec

  let set_frame_size t m = t.frame_size <- m
  let set_max_ip_mtu t m = t.frame_size <- m + Ethernet.Packet.sizeof_ethernet

  let create ~frame_size ~sw () =
    let xt = X.create ~use_async_readers:sw () in
    { xt; frame_size }

  let create ~sw ~clock:_ () =
    create ~sw ~frame_size:(1500 + Ethernet.Packet.sizeof_ethernet) ()
end

(** This backend adds a random number of trailing bytes to each frame *)
module Trailing_bytes : Backend = struct
  module X = Basic_backend.Make
  include X

  let max_bytes_to_add = 10

  (* Just adds trailing bytes, doesn't store anything in them *)
  let add_random_bytes src =
    let bytes_to_add = Random.int max_bytes_to_add in
    let len = Cstruct.length src in
    let dst = Cstruct.create (len + bytes_to_add) in
    Cstruct.blit src 0 dst 0 len;
    dst

  let set_listen_fn t id fn =
    (* Add random bytes before returning result to real listener *)
    X.set_listen_fn t id (fun buf -> fn (add_random_bytes buf))

  let create ~sw ~clock:_ () = X.create ~use_async_readers:sw ()
end

(** This backend drops packets *)
module Uniform_packet_loss : Backend = struct
  module X = Basic_backend.Make
  include X

  let drop_p = 0.01

  let writev t id iovec =
    if Random.float 1.0 < drop_p then (
      MProf.Trace.label "pkt_drop";
      () (* drop packet *))
    else X.writev t id iovec (* pass to real write *)

  let create ~sw ~clock:_ () = X.create ~use_async_readers:sw ()
end

(** This backend uniformly drops packets with no payload *)
module Uniform_no_payload_packet_loss : Backend = struct
  module X = Basic_backend.Make
  include X

  (* We assume that packets with payload are usually filled. We could make the
   * payload check more accurate by parsing the packet properly. *)
  let no_payload_len = 100

  (* Drop probability, if no payload *)
  let drop_p = 0.10

  let writev t id iovec =
    let size = Cstruct.lenv iovec in
    if size <= no_payload_len && Random.float 1.0 < drop_p then (
      MProf.Trace.label "pkt_drop";
      () (* drop packet *))
    else X.writev t id iovec (* pass to real write *)

  let create ~sw ~clock:_ () = X.create ~use_async_readers:sw ()
end

(** This backend drops packets for 1 second after 1 megabyte has been
 * transferred *)
module Drop_1_second_after_1_megabyte : Backend = struct
  module X = Basic_backend.Make

  type t = {
    xt : X.t;
    mutable sent_bytes : int;
    mutable is_dropping : bool;
    mutable done_dropping : bool;
    perform_drop : unit Eio.Condition.t;
  }

  type macaddr = X.macaddr
  type buffer = X.buffer
  type id = X.id

  let byte_limit : int = 1_000_000
  let time_to_sleep : float = 1.0
  let register t = X.register t.xt
  let unregister t id = X.unregister t.xt id
  let mac t id = X.mac t.xt id
  let set_listen_fn t id buf = X.set_listen_fn t.xt id buf
  let unregister_and_flush t id = X.unregister_and_flush t.xt id

  let should_drop t =
    if
      t.sent_bytes > byte_limit && t.is_dropping = false
      && t.done_dropping = false
    then (
      Eio.Condition.broadcast t.perform_drop ();
      true)
    else if t.is_dropping = true then true
    else false

  let writev t id iovec =
    let size = Cstruct.lenv iovec in
    t.sent_bytes <- t.sent_bytes + size;
    if should_drop t then ()
    else X.writev t.xt id iovec (* pass to real write *)

  let create ~sw ~clock () =
    let xt = X.create ~use_async_readers:sw () in
    let perform_drop = Eio.Condition.create ~label:"vnetif_tests.perform_drop_mutex" () in
    let t =
      {
        xt;
        done_dropping = false;
        is_dropping = false;
        sent_bytes = 0;
        perform_drop;
      }
    in
    Eio.Fiber.fork ~sw (fun () ->
        Eio.Condition.await perform_drop;
        Logs.info (fun f ->
            f "Backend dropping packets for %f sec" time_to_sleep);
        t.is_dropping <- true;
        Eio.Time.sleep clock time_to_sleep;
        t.done_dropping <- true;
        t.is_dropping <- false;
        Logs.info (fun f -> f "Stopped dropping"));
    t
end

(** This backend has a global on/off switch which drops all the packets *)
module On_off_switch = struct
  module X = Basic_backend.Make
  include X

  let send_packets = ref true

  let writev t id iovec =
    if not !send_packets then (
      Logs.info (fun f -> f "write dropping 1 packet");
      MProf.Trace.label "pkt_drop";
      () (* drop packet *))
    else X.writev t id iovec (* pass to real write *)

  let create ~sw ~clock:_ () = X.create ~use_async_readers:sw ()
end

(** This backend delivers all packets unmodified *)
module Basic : Backend = struct
  module X = Basic_backend.Make
  include X

  let create ~sw ~clock:_ () = X.create ~use_async_readers:sw ()
end
