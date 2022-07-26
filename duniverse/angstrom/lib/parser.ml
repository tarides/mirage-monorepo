exception Fail of string list * string

type reader = int -> (Bigstringaf.t * int * int * More.t)

type state = {
  read : reader;
  mutable input : Input.t;
  mutable pos : int;
  mutable more : More.t;
}

type 'a t = state -> 'a

 
let run p state =
  match p state with
  | x ->
    Exported_state.Done (state.pos - Input.client_committed_bytes state.input, x)
  | exception Fail (sl, s) -> Exported_state.Fail (state.pos - Input.client_committed_bytes state.input, sl, s)

let parse ~read p =
  run p {
    read;
    input = Input.create Bigstringaf.empty ~committed_bytes:0 ~off:0 ~len:0;
    pos = 0;
    more = Incomplete;
  }

let parse_bigstring p buf =
  let len = Bigstringaf.length buf in
  let state = {
    read = (fun _ -> assert false);             (* Always complete *)
    input = Input.create buf ~committed_bytes:0 ~off:0 ~len;
    pos = 0;
    more = Complete;
  } in
  Exported_state.state_to_result (run p state)

module Monad = struct
  let return v _ = v

  let fail msg _state = raise_notrace (Fail ([], msg))

  let (>>=) p f state =
    let v = p state in
    f v state

  let (>>|) p f state =
    let v = p state in
    f v

  let (<$>) f m =
    m >>| f

  let (<*>) f m state =
    (* f >>= fun f -> m >>| f *)
    let f = f state in
    let m = m state in
    f m

  let lift f m =
    f <$> m

  let lift2 f m1 m2 state =
    let m1 = m1 state in
    let m2 = m2 state in
    f m1 m2

  let lift3 f m1 m2 m3 state =
    let m1 = m1 state in
    let m2 = m2 state in
    let m3 = m3 state in
    f m1 m2 m3

  let lift4 f m1 m2 m3 m4 state =
    let m1 = m1 state in
    let m2 = m2 state in
    let m3 = m3 state in
    let m4 = m4 state in
    f m1 m2 m3 m4

  let ( *>) a b state =
    (* a >>= fun _ -> b *)
    let _ = a state in
    b state

  let (<* ) a b state =
    (* a >>= fun x -> b >>| fun _ -> x *)
    let x = a state in
    let _ = b state in
    x
end

module Choice = struct
  let (<?>) p mark state =
    try p state
    with Fail (marks, msg) ->
      raise_notrace (Fail ((mark::marks), msg))

  let (<|>) p q state =
    let old_pos = state.pos in
    try p state
    with Fail _ as ex ->
      (* The only three constructors that catch [Fail] are [<?>], [<|>] and [many].
       * If the initial input position is less than the length
       * of the committed input, then calling the failure continuation will
       * have the effect of unwinding all choices and collecting marks along
       * the way. *)
      if old_pos < Input.parser_committed_bytes state.input then
        raise_notrace ex
      else (
        state.pos <- old_pos;
        q state
      )
end

module Monad_use_for_debugging = struct
  let return = Monad.return
  let fail   = Monad.fail
  let (>>=)  = Monad.(>>=)

  let (>>|) m f = m >>= fun x -> return (f x)

  let (<$>) f m = m >>| f
  let (<*>) f m = f >>= fun f -> m >>| f

  let lift  = (>>|)
  let lift2 f m1 m2       = f <$> m1 <*> m2
  let lift3 f m1 m2 m3    = f <$> m1 <*> m2 <*> m3
  let lift4 f m1 m2 m3 m4 = f <$> m1 <*> m2 <*> m3 <*> m4

  let ( *>) a b = a >>= fun _ -> b
  let (<* ) a b = a >>= fun x -> b >>| fun _ -> x
end
