(** [parse s] parses [s] into an AST. *)
let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val] converts [e] to a string. *)
let string_of_val (e : Ast.expr) : string =
  match e with
  | Ast.Int i -> string_of_int i
  | Ast.Command cmd -> cmd
  | Ast.Meta_Command mcmd -> mcmd
  | Ast.Binop _ ->
      failwith
        "Interpreting issue, should never pass binary operation as a single \
         value"

(** [is_value e] checks if e is a value or not. Returns bool *)
let is_value e : bool =
  match e with
  | Ast.Int _ -> true
  | Ast.Command _ -> true
  | Ast.Meta_Command _ -> true
  | Ast.Binop _ -> false

(** [step e] takes a single step of eval of [e]*)
let rec step e : Ast.expr =
  match e with
  | Ast.Int _ -> failwith "Int should already be a value and doesn't step!"
  | Ast.Command _ -> failwith "TODO"
  | Ast.Meta_Command _ -> failwith "TODO"
  | Ast.Binop (op, e1, e2) when is_value e1 && is_value e2 ->
      binop_step op e1 e2
  | Ast.Binop (op, e1, e2) when is_value e1 -> Ast.Binop (op, e1, step e2)
  | Ast.Binop (op, e1, e2) -> Ast.Binop (op, step e1, e2)

and binop_step (bop : Ast.binop) (e1 : Ast.expr) (e2 : Ast.expr) =
  match (bop, e1, e2) with
  | Ast.Add, Ast.Int i1, Ast.Int i2 -> Ast.Int (i1 + i2)
  | _ -> failwith "Not a Binary operations!"

(** [eval e] fully evaluates [e] to a value of [v]. *)
let rec eval (e : Ast.expr) : Ast.expr =
  if is_value e then
    e
  else
    eval (step e)

(** [interpreta s] interprets [s] by lexing + parsing it, evaluating it, and
    converting it to a string*)
let interpreta (s : string) : string = s |> parse |> eval |> string_of_val
