open SQCaml

let with_temp_dir f =
  (* let base = Filename.get_temp_dir_name () in *)
  let dirname = Filename.temp_file "sqcaml_test_" "" in
  Sys.remove dirname;
  Unix.mkdir dirname 0o755;
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists dirname then
        Unix.rmdir dirname)
    (fun () -> f dirname)

let tmpdb = Db_session.open_db "test_btree.db"

let test_helper (exec : Interpreter.execution_t) : string =
  match exec with
  | Interpreter.Quit -> ".exit"
  | Interpreter.Ok -> "okay"
  | Interpreter.Message s -> s
  | Interpreter.Error err -> err
  | Interpreter.Help _ -> "help" (* need to improve this *)

let test_bsearch () =
  let _ =
    test_helper
      (Interpreter.interpret tmpdb
         "INSERT INTO Mbta (Id, stop_name, rail_line) VALUES (1, 'f', 'g')")
  in
  let _ =
    test_helper
      (Interpreter.interpret tmpdb
         "INSERT INTO Mbta (Id, stop_name, rail_line) VALUES (99, 'b', 'r')")
  in
  let _ =
    test_helper
      (Interpreter.interpret tmpdb
         "INSERT INTO Mbta (Id, stop_name, rail_line) VALUES (2, 'c', 'b')")
  in
  let res = test_helper (Interpreter.interpret tmpdb ".tree") in

  let correct = "leaf (size 3)\n- 1\n- 2" in
  Alcotest.(check bool)
    "Correct ID sorting" true
    (String.starts_with ~prefix:correct res)

let test_dupes () =
  let _ =
    test_helper
      (Interpreter.interpret tmpdb
         "INSERT INTO Mbta (Id, stop_name, rail_line) VALUES (1, 'f', 'g')")
  in
  let res =
    test_helper
      (Interpreter.interpret tmpdb
         "INSERT INTO Mbta (Id, stop_name, rail_line) VALUES (1, 'b', 'r')")
  in

  Alcotest.(check bool)
    "Correct duplicate handling" true
    (String.starts_with ~prefix:"duplicate" res)

let tests =
  [
    Alcotest.test_case "Correct sorting" `Quick test_bsearch;
    Alcotest.test_case "Correct dupe handling" `Quick test_dupes;
  ]
