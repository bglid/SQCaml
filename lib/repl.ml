(* type MetaCommandResult =  *)
(*         | Meta_Command_SUCCESS *)

(* NEED TO CHANGE TO Interpreter *)
let rec repl_loop () =
  Printf.printf "\nSQCaml > %!";
  let command = read_line () in
  let parsed_command = Interpreter.parse command in
  match parsed_command with
  | Meta_Command mc -> (
      match mc with
      | ".exit" ->
          Printf.printf "exiting SQCaml...\n";
          ()
      | _ ->
          Printf.printf "Meta command not implemented yet...\n";
          repl_loop ())
  | Command com -> (
      match com with
      | "help" ->
          Printf.printf "help \t *Prints help info for repl commands*\n";
          Printf.printf ".exit \t *Exits SQCaml*\n";
          repl_loop ()
      | _ ->
          Printf.printf "Command not implemented yet";
          repl_loop ())
  | Int n ->
      Printf.printf "Entered: %d" n;
      repl_loop ()
  | Float n ->
      Printf.printf "Entered: %f" n;
      repl_loop ()
  (* bad and hacky for now*)
  | Binop (_, _, _) ->
      let bop_expr = Interpreter.interpreta command in
      Printf.printf "%s" bop_expr;
      repl_loop ()

let start () = repl_loop ()
