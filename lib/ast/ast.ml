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

(*NOTE: MOVE TO OWN MODULE*)
type create_t = {
  table_name : string;
  columns : string list;
}
[@@deriving show]
(** Handles creating new tables *)

(** Statments like INSERT *)
type statement =
  | Create of create_t
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
