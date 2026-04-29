(** binary operations like + and *, math! *)
type binop_t =
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
  | String of string
  | Binop of binop_t * expr * expr
[@@deriving show]

(** Statments like INSERT *)
type statement =
  | Insert of Insert.t
  | Select of Select.t
  | Expr of expr
  | Unk_stmt of string
[@@deriving show]

type meta_command =
  | Help
  | Exit
  | Tree
  | Unk_mcmd of string

type top_level =
  | Statement of statement
  | Meta_command of meta_command
