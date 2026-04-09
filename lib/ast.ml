(** binary operations like + and *, math! *)
type binop =
  | Add
  | Subt
  | Mult
  | Div
[@@deriving show]

(** Our Abstract Syntax Tree for interpreter, hooray! *)
type expr =
  | Int of int
  | Float of float
  | Command of string
  | Meta_Command of string
  | Binop of binop * expr * expr
[@@deriving show]
