(** binary operations like + and *, math! *)
type binop =
  | Add
  | Subt
  | Mult
  | Div
  | Lt
  | Gt
  | Leq
  | Geq
  | Neq
  | Comp
[@@deriving show]

(** Our Abstract Syntax Tree for interpreter, hooray! *)
type expr =
  | Int of int
  | Float of float
  | Bool of bool
  | Command of string
  | Meta_Command of string
  | Binop of binop * expr * expr
[@@deriving show]
