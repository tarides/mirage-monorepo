(* int is bytes consumed *)
type 'a parse_result =
  | Done    of int * 'a
  | Fail    of int * string list * string


let state_to_option x = match x with
  | Done(_, v) -> Some v
  | Fail _     -> None

let fail_to_string marks err =
  String.concat " > " marks ^ ": " ^ err

let state_to_result x = match x with
  | Done(_, v)          -> Ok v
  | Fail(_, marks, err) -> Error (fail_to_string marks err)
