(** Used to represent non AST plain values in SQL queries. More of help than
    anything*)

type t =
  | ConstInt of Int32.t
  | ConstStr of string
[@@deriving show]

let make_int (i32 : Int32.t) : t = ConstInt i32
let make_string (str : string) : t = ConstStr str
