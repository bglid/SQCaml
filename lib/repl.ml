open Printf

let rec repl_loop () =
  printf "\nSQCaml > %!";
  let input = read_line () in
  let interpreted_input = Interpreter.interpret input in
  match interpreted_input with
  | Interpreter.Ok -> repl_loop ()
  | Interpreter.Quit ->
      printf "exiting SQCaml...\n";
      ()
  | Interpreter.Help l ->
      List.iter (printf "%s") l;
      (* printf "\n"; *)
      repl_loop ()
  | Interpreter.Message m ->
      printf "%s" m;
      printf "\n";
      repl_loop ()
  | Interpreter.Error err ->
      printf "%s" err;
      printf "\n";
      repl_loop ()

let start () = repl_loop ()
