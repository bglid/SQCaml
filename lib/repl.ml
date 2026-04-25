open Printf

(** Reads a query and only executes statement/expr upon seeing ';' Allows for
    multi-line inputs, accumulating the strings passed in.*)
let rec read_query (acc : string) : string =
  let input = read_line () in
  let acc =
    if acc = "" then
      input
    else
      acc ^ "\n" ^ input
  in
  if
    String.length input >= 1
    && String.sub input (String.length input - 1) 1 = ";"
  then
    acc
  else
    read_query acc

(** Strips the final ';' in [s] before passing it to the lexer *)
let strip_double_semicolon s =
  let len = String.length s in
  if len >= 1 && String.sub s (len - 1) 1 = ";" then
    String.sub s 0 (len - 1)
  else
    s
(* The parser doesn't know what to do with ';'
   To keep it easier I just strip it before handing it to the lexer-parser*)

let rec repl_loop (db : Db_session.t) =
  printf "\nSQCaml > %!";
  let input = strip_double_semicolon (read_query "") in
  let interpreted_input = Interpreter.interpret input in
  match interpreted_input with
  | Interpreter.Ok -> repl_loop db
  | Interpreter.Quit ->
      printf "exiting SQCaml...\n";
      ()
  | Interpreter.Help l ->
      List.iter (printf "%s") l;
      repl_loop db
  | Interpreter.Message m ->
      printf "%s" m;
      printf "\n";
      repl_loop db
  | Interpreter.Error err ->
      printf "%s" err;
      printf "\n";
      repl_loop db

let start (db : Db_session.t) = repl_loop db
