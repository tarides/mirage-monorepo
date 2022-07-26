type t

val create : int -> t
val of_bigstring : off:int -> len:int -> Bigstringaf.t -> t

val feed_string    : t -> off:int -> len:int -> string -> unit
val feed_bigstring : t -> off:int -> len:int -> Bigstringaf.t -> unit
val feed_input : t -> [ `String of string | `Bigstring of Bigstringaf.t ] -> unit

val feed_fn : t -> (Cstruct.t -> int) -> unit
(** [feed_fn t fn] asks [fn] to read into a cstruct. [fn] should return the number of bytes written.
    [fn] should raise [End_of_file] if there is no more data. *)

val shift : t -> int -> unit

val for_reading : t -> Cstruct.t

type unconsumed =
  { buf : Bigstringaf.t
  ; off : int
  ; len : int }

val unconsumed    : ?shift:int -> t -> unconsumed
val of_unconsumed : unconsumed -> t
