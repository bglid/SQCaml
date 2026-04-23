open SQCaml

(* based on type row_t = {id : int; stop_name : string; rail_line : string} *)
let with_temp_file f =
  let filename = Filename.temp_file "sqcaml_test_" ".db" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists filename then
        Sys.remove filename)
    (fun () -> f filename)

let test_insert_and_select_one () =
  with_temp_file @@ fun filename ->
  let table = Table.open_table filename in
  let row = { Ast.id = 1; stop_name = "englewood_avenue"; rail_line = "G" } in

  match Table.insert table row with
  | Error e -> Alcotest.failf "insert failed: %s" e
  | Ok () ->
      let rows = Table.select_all table in
      Alcotest.(check int) "one row returned" 1 (List.length rows);
      let res = List.hd rows in
      Alcotest.(check int) "id preserved" 1 res.id;
      Alcotest.(check string) "stop preserved" "englewood_avenue" res.stop_name;
      Alcotest.(check string) "rail preserved" "G" res.rail_line

let test_duplicate_key_rejected () =
  with_temp_file @@ fun filename ->
  let table = Table.open_table filename in
  let row = { Ast.id = 1; stop_name = "englewood_avenue"; rail_line = "G" } in

  match Table.insert table row with
  | Error e -> Alcotest.failf "first insert failed unexpectedly: %s" e
  | Ok () -> (
      match Table.insert table row with
      | Ok () -> Alcotest.fail "duplicate insert should have failed"
      | Error _ -> ())

let test_select_returns_sorted_rows () =
  with_temp_file @@ fun filename ->
  let table = Table.open_table filename in
  let rows =
    [
      { Ast.id = 3; stop_name = "englewood_avenue"; rail_line = "G" };
      { Ast.id = 1; stop_name = "clevelend_circle"; rail_line = "G" };
      { Ast.id = 2; stop_name = "maverick"; rail_line = "B" };
    ]
  in
  List.iter
    (fun row ->
      match Table.insert table row with
      | Ok () -> ()
      | Error e -> Alcotest.failf "insert failed: %s" e)
    rows;

  let res = Table.select_all table |> List.map (fun r -> r.id) in
  Alcotest.(check (list int)) "ids returned in sorted order" [ 1; 2; 3 ] res

let test_persistence_after_reopen () =
  with_temp_file @@ fun filename ->
  let row = { Ast.id = 1; stop_name = "englewood_avenue"; rail_line = "G" } in

  let table1 = Table.open_table filename in
  begin match Table.insert table1 row with
  | Ok () -> ()
  | Error e -> Alcotest.failf "insert failed: %s" e
  end;
  Table.close table1;

  let table2 = Table.open_table filename in
  let rows = Table.select_all table2 in
  Alcotest.(check int) "one row after reopen" 1 (List.length rows);
  let res = List.hd rows in
  Alcotest.(check int) "id preserved after reopen" 1 res.id;
  Alcotest.(check string)
    "name preserved after reopen" "englewood_avenue" res.stop_name

let tests =
  [
    Alcotest.test_case "insert/select one" `Quick test_insert_and_select_one;
    Alcotest.test_case "duplicate key rejected" `Quick
      test_duplicate_key_rejected;
    Alcotest.test_case "select sorted rows" `Quick
      test_select_returns_sorted_rows;
    Alcotest.test_case "persistence after reopen" `Quick
      test_persistence_after_reopen;
  ]
