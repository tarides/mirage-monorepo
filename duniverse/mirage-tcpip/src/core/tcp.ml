module Keepalive = struct
  type t = {
    after: Duration.t;
    interval: Duration.t;
    probes: int;
  }
end

exception Refused
exception Timeout

module type S = sig
  type ipaddr
  type t
  val disconnect : t -> unit

  type dst = <dst: ipaddr * int>

  val create_connection: ?keepalive:Keepalive.t -> t -> ipaddr * int -> <Eio.Flow.two_way; Eio.Flow.close; dst>
  val listen : t -> port:int -> ?keepalive:Keepalive.t -> (<Eio.Flow.two_way; Eio.Flow.close; dst> -> unit) -> unit
  val unlisten : t -> port:int -> unit

  val input: t -> src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
end
