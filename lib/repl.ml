let rec repl_loop () =
  Printf.printf "\nSQCaml > %!";
  let command = read_line () in
  match command with
  | "exit" ->
      Printf.printf "exiting SQCaml...\n";
      ()
  | "help" | "-h" | "--h" ->
      Printf.printf "help, -h, --h:\t *Prints help info for repl commands*\n";
      Printf.printf "exit:\t *Exits SQCaml*\n";
      repl_loop ()
  | x ->
      Printf.printf "Unrecognized command >>> %s <<<\n" x;
      repl_loop ()

let start () = repl_loop ()
