let () =
  Printf.printf "*********SQCaml*********\n\r";
  Printf.printf "SQCaml > ";
  let repl = read_line () in
  Printf.printf "You entered: %s \n" repl
