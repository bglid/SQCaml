(** binary operations like + and *, math! *)
type binop =
  | Add
  | Mult

(** Our Abstract Syntax Tree for interpreter, hooray! *)
type expr =
  | Int of int
  | Command of string
  | Meta_Command of string
  | Binop of binop * expr * expr
