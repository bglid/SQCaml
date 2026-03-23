(* type MetaCommandResult =  *)
(*         | META_COMMAND_SUCCESS *)

let rec repl_loop () =
  Printf.printf "\nSQCaml > %!";
  let command = read_line () in
  let parsed_command = Interpreter.parse command in
  match parsed_command with
  | META_COMMAND mc -> (
      match mc with
      | ".exit" ->
          Printf.printf "exiting SQCaml...\n";
          ()
      | _ ->
          Printf.printf "Meta command not implemented yet...\n";
          repl_loop ())
  | COMMAND com -> (
      match com with
      | "help" | "-h" | "--h" ->
          Printf.printf
            "help, -h, --h:\t *Prints help info for repl commands*\n";
          Printf.printf ".exit:\t *Exits SQCaml*\n";
          repl_loop ()
      | _ ->
          Printf.printf "Command not implemented yet";
          repl_loop ())
  | Int n ->
      Printf.printf "Entered: %d" n;
      repl_loop ()

let start () = repl_loop ()
