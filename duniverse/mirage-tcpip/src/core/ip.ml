(** can't send a message to that destination *)
exception No_route of string 
exception Would_fragment


type proto = [ `TCP | `UDP | `ICMP ]
let pp_proto ppf = function
  | `TCP -> Fmt.string ppf "TCP"
  | `UDP -> Fmt.string ppf "UDP"
  | `ICMP -> Fmt.string ppf "ICMP"

module type S = sig
  type ipaddr
  val pp_ipaddr : ipaddr Fmt.t
  type t
  val disconnect : t -> unit
  type callback = src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
  val input:
    t ->
    tcp:callback -> udp:callback -> default:(proto:int -> callback) ->
    Cstruct.t -> unit
  val write: t -> ?fragment:bool -> ?ttl:int ->
    ?src:ipaddr -> ipaddr -> proto -> ?size:int -> (Cstruct.t -> int) ->
    Cstruct.t list -> unit
  val pseudoheader : t -> ?src:ipaddr -> ipaddr -> proto -> int -> Cstruct.t
  val src: t -> dst:ipaddr -> ipaddr
  val get_ip: t -> ipaddr list
  val mtu: t -> dst:ipaddr -> int
end
