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
  | Binop of binop * expr * expr
[@@deriving show]

(* plugging in temps for now *)

(** Statments like INSERT *)
type statement =
  | Insert of expr
  | Select of expr
  | Expr of expr
  | Unk_stmt of string
[@@deriving show]

type meta_command =
  | Help
  | Exit
  | Unk_mcmd of string
[@@deriving show]

type top_level =
  | Statement of statement
  | Meta_command of meta_command
[@@deriving show]
