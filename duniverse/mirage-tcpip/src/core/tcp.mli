(** Configuration for TCP keep-alives.
    Keep-alive messages are probes sent on an idle connection. If no traffic
    is received after a certain number of probes are sent, then the connection
    is assumed to have been lost. *)
module Keepalive: sig
  type t = {
    after: Duration.t;    (** initial delay before sending probes on an idle
                              connection *)
    interval: Duration.t; (** interval between successive probes *)
    probes: int;          (** total number of probes to send before assuming
                              that, if the connection is still idle it has
                              been lost *)
  }
  (** Configuration for TCP keep-alives *)
end

exception Refused 
exception Timeout

(** Transmission Control Protocol layer: reliable ordered streaming
    communication. *)
module type S = sig

  type ipaddr
  (** The type for IP address representations. *)

  type t
  (** The type representing the internal state of the TCP layer. *)

  type dst = <dst: ipaddr * int>

  val disconnect: t -> unit
  (** Disconnect from the TCP layer. While this might take some time to
      complete, it can never result in an error. *)

  val create_connection: ?keepalive:Keepalive.t -> t -> ipaddr * int -> <Eio.Flow.two_way; Eio.Flow.close; dst>
  (** [create_connection ~keepalive t (addr,port)] opens a TCP connection
      to the specified endpoint.

      If the optional argument [?keepalive] is provided then TCP keep-alive
      messages will be sent to the server when the connection is idle. If
      no responses are received then eventually the connection will be disconnected:
      [read] will return [Ok `Eof] and write will return [Error `Closed] *)

  val listen : t -> port:int -> ?keepalive:Keepalive.t -> (<Eio.Flow.two_way; Eio.Flow.close; dst> -> unit) -> unit
  (** [listen t ~port ~keepalive callback] listens on [port]. The [callback] is
      executed for each flow that was established. If [keepalive] is provided,
      this configuration will be applied before calling [callback].

      @raise Invalid_argument if [port < 0] or [port > 65535]
 *)

  val unlisten : t -> port:int -> unit
  (** [unlisten t ~port] stops any listener on [port]. *)

  val input: t -> src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
  (** [input t] returns an input function continuation to be
      passed to the underlying {!IP} layer. *)
end
