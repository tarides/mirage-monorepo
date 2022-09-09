(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
 * Copyright (c) 2018-2019 Hannes Mehnert <hannes@mehnert.org>
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

(** Network devices

    [Mirage_net] defines the signature for MirageOS network devices.

    {e Release %%VERSION%% } *)

module Net : sig
  exception Invalid_length of int
end

type stats = {
  mutable rx_bytes : int64;
  mutable rx_pkts : int32;
  mutable tx_bytes : int64;
  mutable tx_pkts : int32;
}
(** The type for frame statistics to track the usage of the device. *)

(** {2 Networking} *)

type t = <
  Eio.Flow.two_way;
  mac : Macaddr.t;
  mtu : int;
  get_stats_counters : stats;
  reset_stats_counters : unit;
>

module Stats : sig
  val create : unit -> stats
  (** [create ()] returns a fresh set of zeroed counters *)

  val rx : stats -> int64 -> unit
  (** [rx t size] records that we received a packet of length [size] *)

  val tx : stats -> int64 -> unit
  (** [tx t size] records that we transmitted a packet of length [size] *)

  val reset : stats -> unit
  (** [reset t] resets all packet counters in [t] to 0 *)
end
