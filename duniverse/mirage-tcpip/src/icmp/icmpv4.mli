(** {2 ICMP layer} *)

(** Internet Control Message Protocol: error messages and operational
    information. *)
module type S = sig

  type t
  (** The type representing the internal state of the ICMP layer. *)

  val disconnect: t -> unit
  (** Disconnect from the ICMP layer. While this might take some time to
      complete, it can never result in an error. *)

  type ipaddr = Ipaddr.V4.t
  (** The type for IP addresses. *)

  val input : t -> src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
  (** [input t src dst buffer] reacts to the ICMP message in
      [buffer]. *)

  val write : t -> ?src:ipaddr -> dst:ipaddr -> ?ttl:int -> Cstruct.t -> unit
  (** [write t ~src ~dst ~ttl buffer] sends the ICMP message in [buffer] to [dst]
      over IP. Passes the time-to-live ([ttl]) to the IP stack if given. *)
end

module Make (I : Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t) : sig
  include S

  val connect : I.t -> t
end
