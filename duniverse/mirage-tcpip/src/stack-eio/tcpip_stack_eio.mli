module Make(Stack: Tcpip.Stack.V4V6) : sig
  val net : Stack.t -> Eio.Net.t
end
