(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module Bigarray = struct
  (* Do not access Bigarray operations directly. If anything's needed, refer to
   * the internal Bigstring module. *)
end

type bigstring = Bigstringaf.t

module Unbuffered = struct
  include Parser

  include Exported_state

  type more = More.t =
    | Complete
    | Incomplete
end

include Unbuffered
include Parser.Monad
include Parser.Choice

module Buffered = struct
  open Effect

  type _ t += Read : int -> (bigstring * int * int * More.t) t
  let read c = perform (Read c)

  type unconsumed = Buffering.unconsumed =
    { buf : bigstring
    ; off : int
    ; len : int }

  type input =
    [ `Bigstring of bigstring
    | `String    of string ]

  type 'a state =
    | Partial of ([ input | `Eof ] -> 'a state)
    | Done    of unconsumed * 'a
    | Fail    of unconsumed * string list * string

  let from_unbuffered_state buffering = function
    | Unbuffered.Done(consumed, v) ->
      let unconsumed = Buffering.unconsumed ~shift:consumed buffering in
      Done(unconsumed, v)
    | Unbuffered.Fail(consumed, marks, msg) ->
      let unconsumed = Buffering.unconsumed ~shift:consumed buffering in
      Fail(unconsumed, marks, msg)

  let parse ?(initial_buffer_size=0x1000) p =
    if initial_buffer_size < 1 then
      failwith "parse: invalid argument, initial_buffer_size < 1";
    let buffering = Buffering.create initial_buffer_size in
    Deep.match_with (Unbuffered.parse ~read) p
      { Deep.retc = from_unbuffered_state buffering;
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) : (((a, 'b state) Deep.continuation) -> 'b state) option ->
          match e with
          | Read committed -> Some (fun k ->
              Buffering.shift buffering committed;
              let cb input =
                let more : More.t =
                  match input with
                  | `Eof            -> Complete
                  | #input as input ->
                    Buffering.feed_input buffering input;
                    Incomplete
                in
                let { Cstruct.buffer; off; len } = Buffering.for_reading buffering in
                Deep.continue k (buffer, off, len, more)
              in
              Partial cb
            )
          | _ -> None
      }

  let feed state input =
    match state with
    | Partial k -> k input
    | Fail(unconsumed, marks, msg) ->
      begin match input with
      | `Eof   -> state
      | #input as input ->
        let buffering = Buffering.of_unconsumed unconsumed in
        Buffering.feed_input buffering input;
        Fail(Buffering.unconsumed buffering, marks, msg)
      end
    | Done(unconsumed, v) ->
      begin match input with
      | `Eof   -> state
      | #input as input ->
        let buffering = Buffering.of_unconsumed unconsumed in
        Buffering.feed_input buffering input;
        Done(Buffering.unconsumed buffering, v)
      end

  let state_to_option = function
    | Done(_, v) -> Some v
    | Partial _  -> None
    | Fail _     -> None

  let state_to_result = function
    | Partial _           -> Error "incomplete input"
    | Done(_, v)          -> Ok v
    | Fail(_, marks, msg) -> Error (Unbuffered.fail_to_string marks msg)

  let state_to_unconsumed = function
    | Done(unconsumed, _)
    | Fail(unconsumed, _, _) -> Some unconsumed
    | Partial _              -> None

end

let failf fmt =
  Printf.ksprintf (fun msg -> raise (Fail([], msg))) fmt

(** BEGIN: getting input *)

let rec prompt state =
  (* [prompt] returns [true] if it has received more input and
   * [false] if there is no chance that the input will grow, i.e., [more = Complete].
   * Otherwise (in the case where the input hasn't grown but [more = Incomplete] just prompt again. *)
  let input = state.Parser.input in
  let parser_uncommitted_bytes = Input.parser_uncommitted_bytes input in
  let parser_committed_bytes   = Input.parser_committed_bytes   input in
  let committed = Input.bytes_for_client_to_commit input in
  let input, off, len, more = state.read committed in
  if len < parser_uncommitted_bytes then
    failwith "prompt: input shrunk!";
  state.input <- Input.create input ~off ~len ~committed_bytes:parser_committed_bytes;
  state.more <- more;
  if len = parser_uncommitted_bytes then
    match (state.more : More.t) with
    | Complete   -> false
    | Incomplete -> prompt state
  else
    true

let demand_input (state : Unbuffered.state) =
  match (state.more : More.t) with
  | Complete   ->
    failf "not enough input"
  | Incomplete ->
    if not (prompt state) then
      failf "not enough input"

let rec ensure_suspended n state =
  demand_input state;
  if state.pos + n > Input.length state.input then
    ensure_suspended n state

let unsafe_apply len ~f state =
  let r = Input.apply state.input state.pos len ~f in
  state.pos <- state.pos + len;
  r

let unsafe_apply_opt len ~f state =
  match Input.apply state.input state.pos len ~f with
  | Error e -> raise (Fail ([], e))
  | Ok    x ->
    state.pos <- state.pos + len;
    x

let ensure n state =
  if state.pos + n > Input.length state.input then ensure_suspended n state

(** END: getting input *)

let at_end_of_input state =
  if state.pos < Input.length state.input then
    false
  else match state.more with
    | Complete -> true
    | Incomplete -> not (prompt state)

let end_of_input =
  at_end_of_input
  >>= function
    | true  -> return ()
    | false -> fail "end_of_input"

let advance n =
  if n < 0 then fail "advance"
  else fun state ->
    ensure n state;
    state.pos <- state.pos + n

let pos state = state.pos

let available state =
  Input.length state.input - state.pos

let commit state =
  Input.commit state.input state.pos

(* Do not use this if [p] contains a [commit]. *)
let unsafe_lookahead p state =
  let old_pos = state.pos in
  let v = p state in
  state.pos <- old_pos;
  v

let peek_char state =
  if state.pos < Input.length state.input then
    Some (Input.unsafe_get_char state.input state.pos)
  else if state.more = Complete then
    None
  else
  if prompt state then
    Some (Input.unsafe_get_char state.input state.pos)
  else
    None

(* This parser is too important to not be optimized. Do a custom job. *)
let rec peek_char_fail state =
  if state.pos < Input.length state.input
  then Input.unsafe_get_char state.input state.pos
  else (
    ensure_suspended 1 state;
    peek_char_fail state
  )

let satisfy f state =
  if state.pos = Input.length state.input then
    ensure_suspended 1 state;
  let c = Input.unsafe_get_char state.input state.pos in
  if f c
  then (
    state.pos <- state.pos + 1;
    c
  ) else (
    failf "satisfy: %C" c
  )

let char c state =
  ensure 1 state;
  if Input.unsafe_get_char state.input state.pos = c
  then (
    state.pos <- state.pos + 1;
    c
  ) else (
    failf "char %C" c
  )

let not_char c state =
  ensure 1 state;
  let c' = Input.unsafe_get_char state.input state.pos in
  if c <> c'
  then (
    state.pos <- state.pos + 1;
    c'
  ) else (
    failf "not char %C" c
  )

let any_char state =
  ensure 1 state;
  let c = Input.unsafe_get_char state.input state.pos in
  state.pos <- state.pos + 1;
  c

let int8 i state =
  ensure 1 state;
  let c = Char.code (Input.unsafe_get_char state.input state.pos) in
  if c = i land 0xff
  then (
    state.pos <- state.pos + 1;
    c
  ) else (
    failf "int8 %d" i state
  )

let any_uint8 state =
  ensure 1 state;
  let c = Input.unsafe_get_char state.input state.pos in
  state.pos <- state.pos + 1;
  Char.code c

let any_int8 =
  (* https://graphics.stanford.edu/~seander/bithacks.html#VariableSignExtendRisky *)
  let s = Sys.int_size - 8 in
  fun state ->
  ensure 1 state;
  let c = Input.unsafe_get_char state.input state.pos in
  state.pos <- state.pos + 1;
  (Char.code c lsl s) asr s

let skip f state =
  ensure 1 state;
  if f (Input.unsafe_get_char state.input state.pos)
  then state.pos <- state.pos + 1
  else failf "skip"

let rec count_while ~init ~f ~with_buffer state =
  let pos         = state.pos in
  let len         = Input.count_while state.input (pos + init) ~f in
  let input_len   = Input.length state.input in
  let init'       = init + len in
  (* Check if the loop terminated because it reached the end of the input
   * buffer. If so, then prompt for additional input and continue. *)
  if pos + init' < input_len || state.more = Complete
  then (
    let r = Input.apply state.input pos init' ~f:with_buffer in
    state.pos <- pos + init';
    r
  ) else (
    if prompt state then
      (count_while ~init:init' ~f ~with_buffer) state
    else (
      let r = Input.apply state.input pos init' ~f:with_buffer in
      state.pos <- pos + init';
      r
    )
  )

let rec count_while1 ~f ~with_buffer state =
  let pos         = state.pos in
  let len         = Input.count_while state.input pos ~f in
  let input_len   = Input.length state.input in
  (* Check if the loop terminated because it reached the end of the input
   * buffer. If so, then prompt for additional input and continue. *)
  if len < 1
  then
    if pos < input_len || state.more = Complete
    then failf "count_while1"
    else if prompt state then
      (count_while1 ~f ~with_buffer) state
    else
      failf "count_while1"
  else if pos + len < input_len || state.more = Complete
  then (
    let r = Input.apply state.input pos len ~f:with_buffer in
    state.pos <- state.pos + len;
    r
  ) else (
    if prompt state then (
      (count_while ~init:len ~f ~with_buffer) state
    ) else (
      let r = Input.apply state.input pos len ~f:with_buffer in
      state.pos <- state.pos + len;
      r
    )
  )

let string_ f s =
  (* XXX(seliopou): Inefficient. Could check prefix equality to short-circuit
   * the io. *)
  let len = String.length s in
  let f buffer ~off ~len =
    let i = ref 0 in
    while !i < len && Char.equal (f (Bigstringaf.unsafe_get buffer (off + !i)))
            (f (String.unsafe_get s !i))
    do
      incr i
    done;
    if len = !i
    then Ok (Bigstringaf.substring buffer ~off ~len)
    else Error "string"
  in
  fun state ->
    ensure len state;
    unsafe_apply_opt len state ~f

let string s    = string_ (fun x -> x) s
let string_ci s = string_ Char.lowercase_ascii s

let skip_while f =
  count_while ~init:0 ~f ~with_buffer:(fun _ ~off:_ ~len:_ -> ())

let take n =
  if n < 0
  then fail "take: n < 0"
  else fun state ->
    ensure n state;
    unsafe_apply n ~f:Bigstringaf.substring state

let take_bigstring n =
  if n < 0
  then fail "take_bigstring: n < 0"
  else fun state ->
    ensure n state;
    unsafe_apply n ~f:Bigstringaf.copy state

let take_bigstring_while f =
  count_while ~init:0 ~f ~with_buffer:Bigstringaf.copy

let take_bigstring_while1 f =
  count_while1 ~f ~with_buffer:Bigstringaf.copy

let take_bigstring_till f =
  take_bigstring_while (fun c -> not (f c))

let peek_string n =
  unsafe_lookahead (take n)

let take_while f =
  count_while ~init:0 ~f ~with_buffer:Bigstringaf.substring

let take_while1 f =
  count_while1 ~f ~with_buffer:Bigstringaf.substring

let take_till f =
  take_while (fun c -> not (f c))

let choice ?(failure_msg="no more choices") ps =
  List.fold_right (<|>) ps (fail failure_msg)

let fix f =
  let rec p = lazy (f r)
  and r state = (Lazy.force p) state
  in
  r

let option x p =
  p <|> return x

let cons x xs = x :: xs

let rec list ps =
  match ps with
  | []    -> return []
  | p::ps -> lift2 cons p (list ps)

let count n p =
  if n < 0 
  then fail "count: n < 0"
  else 
    let rec loop = function
      | 0 -> return []
      | n -> lift2 cons p (loop (n - 1))
    in
    loop n

let rec many p state =
  let old_pos = state.pos in
  match p state with
  | v -> v :: many p state
  | exception (Fail _ as ex) ->
    if old_pos < Input.parser_committed_bytes state.input then
      raise_notrace ex;
    state.pos <- old_pos;
    []

let many1 p =
  lift2 cons p (many p)

let many_till p t =
  fix (fun m ->
    (t *> return []) <|> (lift2 cons p m))

let sep_by1 s p =
  fix (fun m ->
    lift2 cons p ((s *> m) <|> return []))

let sep_by s p =
  (lift2 cons p ((s *> sep_by1 s p) <|> return [])) <|> return []

let rec skip_many p state =
  let old_pos = state.pos in
  match p state with
  | _ -> skip_many p state
  | exception (Fail _ as ex) ->
    if old_pos < Input.parser_committed_bytes state.input then
      raise_notrace ex;
    state.pos <- old_pos

let skip_many1 p =
  p *> skip_many p

let end_of_line =
  (char '\n' *> return ()) <|> (string "\r\n" *> return ()) <?> "end_of_line"

let scan_ state f ~with_buffer parser_state =
  let state = ref state in
  let p =
    count_while ~init:0 ~f:(fun c ->
        match f !state c with
        | None -> false
        | Some state' -> state := state'; true)
      ~with_buffer
    >>| fun x -> x, !state
  in
  p parser_state

let scan state f =
  scan_ state f ~with_buffer:Bigstringaf.substring

let scan_state state f =
  scan_ state f ~with_buffer:(fun _ ~off:_ ~len:_ -> ())
  >>| fun ((), state) -> state

let scan_string state f =
  scan state f >>| fst

let consume_with p f state =
  let start = state.pos in
  let parser_committed_bytes = Input.parser_committed_bytes state.input  in
  let _ = p state in
  if parser_committed_bytes <> Input.parser_committed_bytes state.input
  then failf "consumed: parser committed"
  else (
    let len = state.pos - start in
    Input.apply state.input start len ~f
  )

let consumed           p = consume_with p Bigstringaf.substring
let consumed_bigstring p = consume_with p Bigstringaf.copy

let both a b = lift2 (fun a b -> a, b) a b
let map t ~f = t >>| f
let bind t ~f = t >>= f
let map2 a b ~f = lift2 f a b
let map3 a b c ~f = lift3 f a b c
let map4 a b c d ~f = lift4 f a b c d

module Let_syntax = struct
  let return = return
  let ( >>| ) = ( >>| )
  let ( >>= ) = ( >>= )

  module Let_syntax = struct
    let return = return
    let map = map
    let bind = bind
    let both = both
    let map2 = map2
    let map3 = map3
    let map4 = map4
  end
end

let ( let+ ) = ( >>| )
let ( let* ) = ( >>= )
let ( and+ ) = both

module BE = struct
  (* XXX(seliopou): The pattern in both this module and [LE] are a compromise
   * between efficiency and code reuse. By inlining [ensure] you can recover
   * about 2 nanoseconds on average. That may add up in certain applications.
   *
   * This pattern does not allocate in the fast (success) path.
   * *)
  let int16 n state =
    let bytes = 2 in
    ensure bytes state;
    if Input.unsafe_get_int16_be state.input state.pos = (n land 0xffff)
    then state.pos <- state.pos + bytes
    else failf "BE.int16"

  let int32 n state =
    let bytes = 4 in
    ensure bytes state;
    if Int32.equal (Input.unsafe_get_int32_be state.input state.pos) n
    then state.pos <- state.pos + bytes
    else failf "BE.int32"

  let int64 n state =
    let bytes = 8 in
    ensure bytes state;
    if Int64.equal (Input.unsafe_get_int64_be state.input state.pos) n
    then state.pos <- state.pos + bytes
    else failf "BE.int64"

  let any_uint16 state =
    ensure 2 state;
    unsafe_apply 2 ~f:(fun bs ~off ~len:_ -> Bigstringaf.unsafe_get_int16_be bs off) state

  let any_int16 state =
    ensure 2 state;
    unsafe_apply 2 ~f:(fun bs ~off ~len:_ -> Bigstringaf.unsafe_get_int16_sign_extended_be  bs off) state

  let any_int32 state =
    ensure 4 state;
    unsafe_apply 4 ~f:(fun bs ~off ~len:_ -> Bigstringaf.unsafe_get_int32_be bs off) state

  let any_int64 state =
    ensure 8 state;
    unsafe_apply 8 ~f:(fun bs ~off ~len:_ -> Bigstringaf.unsafe_get_int64_be bs off) state

  let any_float state =
    ensure 4 state;
    unsafe_apply 4 ~f:(fun bs ~off ~len:_ -> Int32.float_of_bits (Bigstringaf.unsafe_get_int32_be bs off)) state

  let any_double state =
    ensure 8 state;
    unsafe_apply 8 ~f:(fun bs ~off ~len:_ -> Int64.float_of_bits (Bigstringaf.unsafe_get_int64_be bs off)) state
end

module LE = struct
  let int16 n state =
    let bytes = 2 in
    ensure bytes state;
    if Input.unsafe_get_int16_le state.input state.pos = (n land 0xffff)
    then state.pos <- state.pos + bytes
    else failf "LE.int16"

  let int32 n state =
    let bytes = 4 in
    ensure bytes state;
    if Int32.equal (Input.unsafe_get_int32_le state.input state.pos) n
    then state.pos <- state.pos + bytes
    else failf "LE.int32"

  let int64 n state =
    let bytes = 8 in
    ensure bytes state;
    if Int64.equal (Input.unsafe_get_int64_le state.input state.pos) n
    then state.pos <- state.pos + bytes
    else failf "LE.int64"

  let any_uint16 state =
    ensure 2 state;
    unsafe_apply 2 ~f:(fun bs ~off ~len:_ -> Bigstringaf.unsafe_get_int16_le bs off) state

  let any_int16  state =
    ensure 2 state;
    unsafe_apply 2 ~f:(fun bs ~off ~len:_ -> Bigstringaf.unsafe_get_int16_sign_extended_le  bs off) state

  let any_int32  state =
    ensure 4 state;
    unsafe_apply 4 ~f:(fun bs ~off ~len:_ -> Bigstringaf.unsafe_get_int32_le bs off) state

  let any_int64 state =
    ensure 8 state;
    unsafe_apply 8 ~f:(fun bs ~off ~len:_ -> Bigstringaf.unsafe_get_int64_le bs off) state

  let any_float state =
    ensure 4 state;
    unsafe_apply 4 ~f:(fun bs ~off ~len:_ -> Int32.float_of_bits (Bigstringaf.unsafe_get_int32_le bs off)) state

  let any_double state =
    ensure 8 state;
    unsafe_apply 8 ~f:(fun bs ~off ~len:_ -> Int64.float_of_bits (Bigstringaf.unsafe_get_int64_le bs off)) state
end

module Unsafe = struct
  let take n f =
    let n = max n 0 in
    fun state ->
    ensure n state;
    unsafe_apply n ~f state

  let peek n f =
    unsafe_lookahead (take n f)

  let take_while check f =
    count_while ~init:0 ~f:check ~with_buffer:f

  let take_while1 check f =
    count_while1 ~f:check ~with_buffer:f

  let take_till check f =
    take_while (fun c -> not (check c)) f
end

module Consume = struct
  type t =
    | Prefix
    | All
end

let parse_bigstring ~consume p bs =
  let p =
    match (consume : Consume.t) with
    | Prefix -> p
    | All -> p <* end_of_input
  in
  Unbuffered.parse_bigstring p bs

let parse_string ~consume p s =
  let len = String.length s in
  let bs  = Bigstringaf.create len in
  Bigstringaf.unsafe_blit_from_string s ~src_off:0 bs ~dst_off:0 ~len;
  parse_bigstring ~consume p bs

let parse ~buffer p read_into =
  let buffering = Buffering.of_unconsumed buffer in
  let read committed =
    Buffering.shift buffering committed;
    let comp =
      match Buffering.feed_fn buffering read_into with
      | exception End_of_file -> Complete
      | () -> Incomplete
    in
    let { Cstruct.buffer; off; len } = Buffering.for_reading buffering in
    (buffer, off, len, comp)
  in
  match Unbuffered.parse ~read p with
  | Unbuffered.Done(consumed, v) ->
    let unconsumed = Buffering.unconsumed ~shift:consumed buffering in
    unconsumed, Ok v
  | Unbuffered.Fail(consumed, marks, msg) ->
    let unconsumed = Buffering.unconsumed ~shift:consumed buffering in
    unconsumed, Error (Unbuffered.fail_to_string marks msg)

let parse_reader ?(initial_buffer_size=0x1000) ~consume p read_into =
  let p =
    match (consume : Consume.t) with
    | Prefix -> p
    | All -> p <* end_of_input
  in
  let buffer = { Buffering.buf = Bigstringaf.create initial_buffer_size; off = 0; len = 0 } in
  snd @@ parse ~buffer p read_into
