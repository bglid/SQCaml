(** [parse s] parses [s] into an AST. *)
let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val] converts [e] to a string. *)
let string_of_val (e : Ast.expr) : string =
  match e with
  | Ast.Int i -> string_of_int i
  | Ast.COMMAND cmd -> cmd
  | Ast.META_COMMAND mcmd -> mcmd

(** [is_value e] checks if e is a value or not. Returns bool *)
let is_value e : bool =
  match e with
  | Ast.Int _ -> true
  | Ast.COMMAND _ -> true
  | Ast.META_COMMAND _ -> true

(** [step e] takes a single step of eval of [e]*)
let step e : Ast.expr =
  match e with
  | Ast.Int _ -> failwith "Int should already be a value and doesn't step!"
  | Ast.COMMAND _ -> failwith "TODO"
  | Ast.META_COMMAND _ -> failwith "TODO"

(** [eval e] fully evaluates [e] to a value of [v]. *)
let rec eval (e : Ast.expr) : Ast.expr =
  if is_value e then
    e
  else
    eval (step e)

(** [interpreta s] interprets [s] by lexing + parsing it, evaluating it, and
    converting it to a string*)
let interpreta (s : string) : string = s |> parse |> eval |> string_of_val
