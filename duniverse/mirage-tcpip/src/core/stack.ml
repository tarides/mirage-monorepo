module type V4 = sig

  type t
  (** The type representing the internal state of the IPv4 stack. *)

  val disconnect: t -> unit
  (** Disconnect from the IPv4 stack. While this might take some time to
      complete, it can never result in an error. *)

  module UDPV4: Udp.S with type ipaddr = Ipaddr.V4.t

  module TCPV4: Tcp.S with type ipaddr = Ipaddr.V4.t

  module IPV4: Ip.S with type ipaddr = Ipaddr.V4.t

  val udpv4: t -> UDPV4.t
  (** [udpv4 t] obtains a descriptor for use with the [UDPV4] module,
      usually to transmit traffic. *)

  val tcpv4: t -> TCPV4.t
  (** [tcpv4 t] obtains a descriptor for use with the [TCPV4] module,
      usually to initiate outgoing connections. *)

  val ipv4: t -> IPV4.t
  (** [ipv4 t] obtains a descriptor for use with the [IPV4] module,
      which can handle raw IPv4 frames, or manipulate IP address
      configuration on the stack interface. *)

  val listen: t -> unit
  (** [listen t] requests that the stack listen for traffic on the
      network interface associated with the stack, and demultiplex
      traffic to the appropriate callbacks. *)
end

module type V6 = sig
  type t
  (** The type representing the internal state of the IPv6 stack. *)

  val disconnect: t -> unit
  (** Disconnect from the IPv6 stack. While this might take some time to
      complete, it can never result in an error. *)

  module UDP: Udp.S with type ipaddr = Ipaddr.V6.t

  module TCP: Tcp.S with type ipaddr = Ipaddr.V6.t

  module IP: Ip.S with type ipaddr = Ipaddr.V6.t

  val udp: t -> UDP.t
  (** [udp t] obtains a descriptor for use with the [UDPV6] module,
      usually to transmit traffic. *)

  val tcp: t -> TCP.t
  (** [tcp t] obtains a descriptor for use with the [TCPV6] module,
      usually to initiate outgoing connections. *)

  val ip: t -> IP.t
  (** [ip t] obtains a descriptor for use with the [IPV6] module,
      which can handle raw IPv6 frames, or manipulate IP address
      configuration on the stack interface. *)

  val listen: t -> unit
  (** [listen t] requests that the stack listen for traffic on the
      network interface associated with the stack, and demultiplex
      traffic to the appropriate callbacks. *)
end

module type V4V6 = sig
  type t
  (** The type representing the internal state of the dual IPv4 and IPv6 stack. *)

  val disconnect: t -> unit
  (** Disconnect from the dual IPv4 and IPv6 stack. While this might take some
      time to complete, it can never result in an error. *)

  module UDP: Udp.S with type ipaddr = Ipaddr.t

  module TCP: Tcp.S with type ipaddr = Ipaddr.t

  module IP: Ip.S with type ipaddr = Ipaddr.t

  val udp: t -> UDP.t
  (** [udp t] obtains a descriptor for use with the [UDP] module,
      usually to transmit traffic. *)

  val tcp: t -> TCP.t
  (** [tcp t] obtains a descriptor for use with the [TCP] module,
      usually to initiate outgoing connections. *)

  val ip: t -> IP.t
  (** [ip t] obtains a descriptor for use with the [IP] module,
      which can handle raw IPv4 and IPv6 frames, or manipulate IP address
      configuration on the stack interface. *)

  val listen: t -> unit
  (** [listen t] requests that the stack listen for traffic on the
      network interface associated with the stack, and demultiplex
      traffic to the appropriate callbacks. *)
end