(** Our Abstract Syntax Tree for interpreter, hooray! *)
type expr =
  | Int of int
  | COMMAND of string
  | META_COMMAND of string
