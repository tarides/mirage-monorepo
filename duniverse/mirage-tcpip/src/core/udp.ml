module type S = sig
  type ipaddr
  type t
  val disconnect : t -> unit
  type callback = src:ipaddr -> dst:ipaddr -> src_port:int -> Cstruct.t -> unit
  val listen : t -> port:int -> callback -> unit
  val unlisten : t -> port:int -> unit
  val input: t -> src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
  val write: ?src:ipaddr -> ?src_port:int -> ?ttl:int -> dst:ipaddr -> dst_port:int -> t -> Cstruct.t ->
    unit
end
