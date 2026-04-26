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

let rec insert_records (db : Db_session.t) (id : int) (n_inserts : int) : unit =
  if id = n_inserts then
    ()
  else
    let insert =
      Insert.make
        [ "id"; "stop_name"; "rail_line" ]
        [
          Constant.make_int (Int32.of_int id);
          Constant.make_string ("stop number: " ^ string_of_int id);
          Constant.make_string "G";
        ]
    in
    ignore (Insert.execute_insert db insert);
    insert_records db (id + 1) n_inserts

let tmpdb = Db_session.open_db ?block_size:(Some 256) "test_btree.db"

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

(* testing dupe check *)
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

(* testing capacity *)
let test_pre_split () =
  let db_dir = "tmp_split_test.db" in
  if Sys.file_exists db_dir then
    Sys.command ("rm -rf" ^ db_dir) |> ignore;

  let db = Db_session.open_db ~block_size:256 db_dir in
  insert_records db 0 31;

  let insert_15 =
    Insert.make
      [ "id"; "stop_name"; "rail_line" ]
      [
        Constant.make_int (Int32.of_int 15);
        Constant.ConstStr "overflow";
        Constant.ConstStr "G";
      ]
  in
  Alcotest.check_raises "Split on capacity"
    (Failure "HELP! please implement splitting plz.") (fun () ->
      ignore (Insert.execute_insert db insert_15));

  Db_session.close_db db

let tests =
  [
    Alcotest.test_case "Correct sorting" `Quick test_bsearch;
    Alcotest.test_case "Correct dupe handling" `Quick test_dupes;
    Alcotest.test_case "Check hitting capacity" `Quick test_pre_split;
  ]
