type execution_t =
  | Quit
  | Ok
  | Help of string list
  | Message of string
  | Error of string

let help_list =
  [
    ".help \t *Prints help info for repl commands*\n";
    ".exit \t *Exits SQCaml*\n";
  ]

(** [parse s] parses [s] into a top level statment/mcommand. *)
let parse (s : string) : Ast.top_level =
  let lexbuf = Lexing.from_string s in
  let res = Parser.prog Lexer.read lexbuf in
  res

(** [string_of_val] converts [e] to a string. *)
let string_of_val (e : Ast.expr) : string =
  match e with
  | Ast.Int i -> string_of_int i
  | Ast.Float f -> string_of_float f
  | Ast.Bool b -> string_of_bool b
  | Ast.String s -> s
  | Ast.Binop _ ->
      failwith
        "Interpreting issue, should never pass binary operation as a single \n\
        \         value"

(** [is_value e] checks if e is a value or not. Returns bool *)
let is_value e : bool =
  match e with
  | Ast.Int _ -> true
  | Ast.Float _ -> true
  | Ast.Bool _ -> true
  | Ast.String _ -> true
  | Ast.Binop _ -> false

(* NOTE: Probably want to generalize handling these, min approach for now*)
let _num_comp_step (bop : Ast.binop_t) e1 e2 =
  match (bop, e1, e2) with
  | Ast.Lt, i1, i2 -> Ast.Bool (i1 < i2)
  | Ast.Gt, i1, i2 -> Ast.Bool (i1 > i2)
  | Ast.Leq, i1, i2 -> Ast.Bool (i1 <= i2)
  | Ast.Geq, i1, i2 -> Ast.Bool (i1 >= i2)
  | Ast.Neq, i1, i2 -> Ast.Bool (i1 <> i2)
  | Ast.Comp, i1, i2 -> Ast.Bool (i1 = i2)
  | _, _, _ ->
      failwith "[ERROR]: Somehow passed non comp step to _num_comp_step"

let _int_step (bop : Ast.binop_t) (e1 : int) (e2 : int) =
  match (bop, e1, e2) with
  | Ast.Add, i1, i2 -> Ast.Int (i1 + i2)
  | Ast.Subt, i1, i2 -> Ast.Int (i1 - i2)
  | Ast.Mult, i1, i2 -> Ast.Int (i1 * i2)
  | Ast.Div, i1, i2 -> Ast.Int (i1 / i2)
  | (Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq | Ast.Neq | Ast.Comp), _, _ ->
      _num_comp_step bop e1 e2

let _float_step (bop : Ast.binop_t) (e1 : float) (e2 : float) =
  match (bop, e1, e2) with
  | Ast.Add, i1, i2 -> Ast.Float (i1 +. i2)
  | Ast.Subt, i1, i2 -> Ast.Float (i1 -. i2)
  | Ast.Mult, i1, i2 -> Ast.Float (i1 *. i2)
  | Ast.Div, i1, i2 -> Ast.Float (i1 /. i2)
  | (Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq | Ast.Neq | Ast.Comp), _, _ ->
      _num_comp_step bop e1 e2

(* let _table_step (table : Ast.table) = *)
(*   match table with *)
(*   | Ast.Id id -> Ast.String id *)

(** [step e] takes a single step of eval of [e]*)
let rec step e : Ast.expr =
  match e with
  | Ast.Int _ -> failwith "Int should already be a value and doesn't step!"
  | Ast.Float _ -> failwith "Float should already be a value and doesn't step!"
  | Ast.Bool _ -> failwith "Bool should already be a value and doesn't step!"
  | Ast.String _ -> failwith "Bool should already be a value and doesn't step!"
  | Ast.Binop (op, e1, e2) when is_value e1 && is_value e2 ->
      binop_step op e1 e2
  | Ast.Binop (op, e1, e2) when is_value e1 -> Ast.Binop (op, e1, step e2)
  | Ast.Binop (op, e1, e2) -> Ast.Binop (op, step e1, e2)
(* | Ast.Table id -> _table_step id *)

(* handles int and float by converting int -> float *)
and binop_step (bop : Ast.binop_t) (e1 : Ast.expr) (e2 : Ast.expr) =
  match (bop, e1, e2) with
  | _, Ast.Int i1, Ast.Int i2 -> _int_step bop i1 i2
  | _, Ast.Float f1, Ast.Float f2 -> _float_step bop f1 f2
  | _, Ast.Int i1, Ast.Float f2 -> _float_step bop (float_of_int i1) f2
  | _, Ast.Float f1, Ast.Int i2 -> _float_step bop f1 (float_of_int i2)
  | Ast.Add, _, _ -> failwith "Not a Binary for addition wth!"
  | Ast.Subt, _, _ -> failwith "Not a Binary for subtraction wth!"
  | Ast.Mult, _, _ -> failwith "Not a Binary for multiplication wth!"
  | Ast.Div, _, _ -> failwith "Not a Binary for division wth!"
  | Ast.Lt, _, _ ->
      failwith "Need numerical values for < operation this isn't JavaScript!"
  | Ast.Gt, _, _ ->
      failwith "Need numerical values for > operation this isn't JavaScript!"
  | Ast.Leq, _, _ ->
      failwith "Need numerical values for < operation this isn't JavaScript!"
  | Ast.Geq, _, _ ->
      failwith "Need numerical values for > operation this isn't JavaScript!"
  | Ast.Neq, _, _ -> failwith "Need to decide on HOW I want to implement this"
  | Ast.Comp, _, _ -> failwith "Need to decide on HOW I want to implement this"

(** [eval e] fully evaluates [e] to a value of [v]. *)
let rec eval (e : Ast.expr) : Ast.expr =
  if is_value e then
    e
  else
    eval (step e)

let execute_meta (md : Ast.meta_command) : execution_t =
  match md with
  | Ast.Exit -> Quit
  | Ast.Help -> Help help_list
  | Ast.Unk_mcmd _ -> Error "unk meta command: Failure"

let execute_statement stmt : execution_t =
  (* Need to improve this once the B+ is implemented *)
  match stmt with
  | Ast.Create _ -> Ok
  | Ast.Insert i -> Message (Insert.execute_insert i)
  | Ast.Select s -> Message (Select.execute_select s)
  | Ast.Expr e -> Message (e |> eval |> string_of_val)
  | Ast.Unk_stmt u -> Message u

(** [interpret input] interprets [input] by lexing + parsing it into a toplevel,
    evaluating it, and converting it to a string*)
let interpret (s : string) =
  let parsed_input = s |> parse in
  match parsed_input with
  | Ast.Meta_command md -> execute_meta md
  | Ast.Statement stmt -> execute_statement stmt
