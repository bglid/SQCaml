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
let test_split () =
  let db_dir = "tmp_split_test.db" in
  if Sys.file_exists db_dir then
    Sys.command ("rm -rf " ^ db_dir) |> ignore;

  let db = Db_session.open_db ~block_size:256 db_dir in
  insert_records db 0 50;

  let root = Btree.get_node db.index db.index.root_num in
  Alcotest.(check int) "Correct Splitting via internal node key" 1 root.cur_size;
  Alcotest.(check bool) "Correct node type" true (root.node_t = Nodes.Internal)

let test_node_type_no_split () =
  let db_dir = "tmp_split_test.db" in
  if Sys.file_exists db_dir then
    Sys.command ("rm -rf " ^ db_dir) |> ignore;

  let db = Db_session.open_db ~block_size:256 db_dir in
  insert_records db 0 10;

  let root = Btree.get_node db.index db.index.root_num in
  Alcotest.(check bool) "Correct node type" true (root.node_t = Nodes.Leaf)

let test_node_split_child () =
  let db_dir = "tmp_split_test.db" in
  if Sys.file_exists db_dir then
    Sys.command ("rm -rf " ^ db_dir) |> ignore;

  let db = Db_session.open_db ~block_size:256 db_dir in
  insert_records db 0 100;

  (* let root = Btree.get_node db.index db.index.root_num in *)
  let cursor = Cursor.tree_find db.index (Keys.Integer (Int32.of_int 1)) in
  let found_node = Btree.get_node cursor.tree cursor.page_num in
  Alcotest.(check bool) "Correct node type" true (found_node.node_t = Nodes.Leaf)

let test_internal_find_after_split () =
  let db_dir = "tmp_split_test.db" in
  if Sys.file_exists db_dir then
    Sys.command ("rm -rf " ^ db_dir) |> ignore;

  let db = Db_session.open_db ~block_size:256 db_dir in
  insert_records db 1 35;

  (*inserting after a split*)
  let _ =
    test_helper
      (Interpreter.interpret db
         "INSERT INTO Mbta (Id, stop_name, rail_line) VALUES (0, 'f', 'g')")
  in
  let _ =
    test_helper
      (Interpreter.interpret db
         "INSERT INTO Mbta (Id, stop_name, rail_line) VALUES (36, 'f', 'g')")
  in

  let root = Btree.get_node db.index db.index.root_num in

  match root.node_t with
  | Nodes.Leaf -> failwith "BAD!!!! Shouldn't be a leaf aftera split"
  | Nodes.Internal -> begin
      Alcotest.(check int) "root has 1 sep" 1 root.cur_size;

      let left_page = root.pointers.(0) in
      let right_page = root.pointers.(1) in

      let left_child = Btree.get_node db.index left_page in
      let right_child = Btree.get_node db.index right_page in

      Alcotest.(check bool)
        "Left child is a leaf" true
        (left_child.node_t = Nodes.Leaf);

      Alcotest.(check bool)
        "Right child is a leaf" true
        (right_child.node_t = Nodes.Leaf);

      (* checkin them vals*)
      Alcotest.(check bool)
        "Left page contains 0" true
        (Array.exists (Keys.equals (Keys.Integer 0l)) left_child.keys);

      Alcotest.(check bool)
        "Right page contains 36" true
        (Array.exists (Keys.equals (Keys.Integer 36l)) right_child.keys);

      (* testing that tree find gives a cursor into a leaf not an internal node*)
      let left_cursor = Cursor.tree_find db.index (Keys.Integer 0l) in
      let right_cursor = Cursor.tree_find db.index (Keys.Integer 36l) in

      Alcotest.(check int)
        "tree_find 0 goes into the left leaf" left_page left_cursor.page_num;

      Alcotest.(check int)
        "tree_find 36 goes into the right leaf" right_page right_cursor.page_num;

      Db_session.close_db db
    end

let tests =
  [
    Alcotest.test_case "Correct sorting" `Quick test_bsearch;
    Alcotest.test_case "Correct dupe handling" `Quick test_dupes;
    Alcotest.test_case "Check splitting capacity & Correct root type" `Quick
      test_split;
    Alcotest.test_case "Check preserving type on no split" `Quick
      test_node_type_no_split;
    Alcotest.test_case "Check children node type" `Quick test_node_split_child;
    Alcotest.test_case "Testing root split partitions to child nodes correctly"
      `Quick test_internal_find_after_split;
  ]
