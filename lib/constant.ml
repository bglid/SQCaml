(** Used to represent non AST plain values in SQL queries. More of help than
    anything*)

exception NotIntConst
exception NotStrConst

type t =
  | ConstInt of Int32.t
  | ConstStr of string
[@@deriving show]

let make_int (i32 : Int32.t) : t = ConstInt i32
let make_string (str : string) : t = ConstStr str

let to_int (constint : t) : Int32.t =
  match constint with
  | ConstInt i32 -> i32
  | ConstStr _ -> raise NotIntConst

let to_str (constint : t) : string =
  match constint with
  | ConstInt _ -> raise NotStrConst
  | ConstStr s -> s
