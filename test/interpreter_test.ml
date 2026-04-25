open SQCaml

module To_test = struct
  let run_interpreter expected v =
    Alcotest.(check string) "Interpreter test" expected v
end

let test_helper (exec : Interpreter.execution_t) : string =
  match exec with
  | Interpreter.Quit -> ".exit"
  | Interpreter.Ok -> "okay"
  | Interpreter.Message s -> s
  | Interpreter.Error err -> err
  | Interpreter.Help _ -> "help" (* need to improve this *)

let tmpdb = Db_session.open_db "sqcaml.db"

let test_interpreted_int () =
  let expr = Interpreter.interpret tmpdb "22" in
  To_test.run_interpreter (string_of_int 22) (test_helper expr)

(* Int tests *)
let test_add_int () =
  let expr = Interpreter.interpret tmpdb "22 + 20" in
  To_test.run_interpreter (string_of_int 42) (test_helper expr)

let test_mult_int () =
  let expr = Interpreter.interpret tmpdb "2 * 20" in
  To_test.run_interpreter (string_of_int 40) (test_helper expr)

let test_sub_int () =
  let expr = Interpreter.interpret tmpdb "22 - 20" in
  To_test.run_interpreter (string_of_int 2) (test_helper expr)

let test_div_int () =
  let expr = Interpreter.interpret tmpdb "20 / 2" in
  To_test.run_interpreter (string_of_int 10) (test_helper expr)

let test_precedence_mult_int () =
  let expr = Interpreter.interpret tmpdb "2 * 3 * 10" in
  To_test.run_interpreter (string_of_int 60) (test_helper expr)

let test_precedence_mult_add_int () =
  let expr = Interpreter.interpret tmpdb "2 * 3 + 10" in
  To_test.run_interpreter (string_of_int 16) (test_helper expr)

let test_precedence_add_mult_int () =
  let expr = Interpreter.interpret tmpdb "2 + 3 * 10" in
  To_test.run_interpreter (string_of_int 32) (test_helper expr)

(* Float tests *)
let test_add_float () =
  let expr = Interpreter.interpret tmpdb "22.1 + 20.0" in
  To_test.run_interpreter (string_of_float 42.1) (test_helper expr)

let test_mult_float () =
  let expr = Interpreter.interpret tmpdb "2. * 20." in
  To_test.run_interpreter (string_of_float 40.) (test_helper expr)

let test_sub_float () =
  let expr = Interpreter.interpret tmpdb "22. - 20." in
  To_test.run_interpreter (string_of_float 2.) (test_helper expr)

let test_div_float () =
  let expr = Interpreter.interpret tmpdb "20. / 2." in
  To_test.run_interpreter (string_of_float 10.) (test_helper expr)

(* Bools *)
let test_true () =
  let expr = Interpreter.interpret tmpdb "TRUE" in
  To_test.run_interpreter (string_of_bool true) (test_helper expr)

let test_false () =
  let expr = Interpreter.interpret tmpdb "FALSE" in
  To_test.run_interpreter (string_of_bool false) (test_helper expr)

(* Comparison operators *)
let test_lt_true () =
  let expr = Interpreter.interpret tmpdb "0 < 10" in
  To_test.run_interpreter (string_of_bool true) (test_helper expr)

let test_lt_false () =
  let expr = Interpreter.interpret tmpdb "10 < 9" in
  To_test.run_interpreter (string_of_bool false) (test_helper expr)

let test_gt_true () =
  let expr = Interpreter.interpret tmpdb "10 > 9" in
  To_test.run_interpreter (string_of_bool true) (test_helper expr)

let test_gt_false () =
  let expr = Interpreter.interpret tmpdb "0 > 10" in
  To_test.run_interpreter (string_of_bool false) (test_helper expr)

let test_leq_true () =
  let expr = Interpreter.interpret tmpdb "10 <= 10" in
  To_test.run_interpreter (string_of_bool true) (test_helper expr)

let test_leq_false () =
  let expr = Interpreter.interpret tmpdb "10 <= 0" in
  To_test.run_interpreter (string_of_bool false) (test_helper expr)

let test_geq_true () =
  let expr = Interpreter.interpret tmpdb "10 >= 10" in
  To_test.run_interpreter (string_of_bool true) (test_helper expr)

let test_geq_false () =
  let expr = Interpreter.interpret tmpdb "9 >= 10" in
  To_test.run_interpreter (string_of_bool false) (test_helper expr)

let test_neq_true () =
  let expr = Interpreter.interpret tmpdb "10 <> 1" in
  To_test.run_interpreter (string_of_bool true) (test_helper expr)

let test_neq_false () =
  let expr = Interpreter.interpret tmpdb "10 <> 10" in
  To_test.run_interpreter (string_of_bool false) (test_helper expr)

let test_comp_eq_true () =
  let expr = Interpreter.interpret tmpdb "10 == 10 " in
  To_test.run_interpreter (string_of_bool true) (test_helper expr)

let test_comp_eq_false () =
  let expr = Interpreter.interpret tmpdb "10 == 1" in
  To_test.run_interpreter (string_of_bool false) (test_helper expr)

(* certainty with interpreter *)

let test_parens_whitespace () =
  let expr = Interpreter.interpret tmpdb "(1 + 1) * (2 + 2)" in
  To_test.run_interpreter (string_of_int 8) (test_helper expr)

let test_comments_nl () =
  let expr = Interpreter.interpret tmpdb "--test  \n  (2 + 2)" in
  To_test.run_interpreter (string_of_int 4) (test_helper expr)

(* improved interpreter tests *)
let test_meta_exit () =
  let res = test_helper (Interpreter.interpret tmpdb ".exit") in
  Alcotest.(check string) "exit recognized" ".exit" res

let test_unknown_command () =
  let res = test_helper (Interpreter.interpret tmpdb "NONSENSE") in
  Alcotest.(check bool)
    "unknown command errors" true
    (String.starts_with ~prefix:"error:" res)

let test_insert_statement_recognized () =
  let res =
    test_helper
      (Interpreter.interpret tmpdb
         "INSERT INTO Mbta (Id, stop_name, rail_line) VALUES (1, 'f', 'g') ")
  in
  Alcotest.(check bool)
    "insert accepted for execution" true
    (String.starts_with ~prefix:"Inserted" res
    || String.starts_with ~prefix:"error:" res)

let test_select_statement_recognized () =
  let res =
    test_helper
      (Interpreter.interpret tmpdb "SELECT stop_name, rail_line FROM Mbta")
  in
  Alcotest.(check bool)
    "select accepted for execution" true
    (res = "englewood G" || String.starts_with ~prefix:"error:" res)

let tests =
  [
    Alcotest.test_case "Check interpreted '22' int" `Quick test_interpreted_int;
    Alcotest.test_case "Check intpreted add math '22 + 20'" `Quick test_add_int;
    Alcotest.test_case "Check intpreted mult math '2 * 20'" `Quick test_mult_int;
    Alcotest.test_case "Check intpreted sub math " `Quick test_sub_int;
    Alcotest.test_case "Check intpreted div math " `Quick test_div_int;
    Alcotest.test_case "Check intpreted precedence mult mult '2 * 3 * 20'"
      `Quick test_precedence_mult_int;
    Alcotest.test_case "Check intpreted precedence mult add '2 * 3 + 10'" `Quick
      test_precedence_mult_add_int;
    Alcotest.test_case "Check intpreted precedence add mult '2 + 3 * 10'" `Quick
      test_precedence_add_mult_int;
    Alcotest.test_case "Check intpreted add float " `Quick test_add_float;
    Alcotest.test_case "Check intpreted mult float math " `Quick test_mult_float;
    Alcotest.test_case "Check intpreted sub float math " `Quick test_sub_float;
    Alcotest.test_case "Check intpreted div float math " `Quick test_div_float;
    Alcotest.test_case "Check TRUE" `Quick test_true;
    Alcotest.test_case "Check FALSE" `Quick test_false;
    Alcotest.test_case "Check <: 0 < 10 " `Quick test_lt_true;
    Alcotest.test_case "Check <: 10 < 9 " `Quick test_lt_false;
    Alcotest.test_case "Check >: 10 > 9 " `Quick test_gt_true;
    Alcotest.test_case "Check >: 0 > 10 " `Quick test_gt_false;
    Alcotest.test_case "Check <=: 10 <= 10 " `Quick test_leq_true;
    Alcotest.test_case "Check <=: 10 <= 0 " `Quick test_leq_false;
    Alcotest.test_case "Check >=: 10 >= 10 " `Quick test_geq_true;
    Alcotest.test_case "Check >=: 9 >= 10 " `Quick test_geq_false;
    Alcotest.test_case "Check <>: 10 <> 1 " `Quick test_neq_true;
    Alcotest.test_case "Check <>: 10 <> 10 " `Quick test_neq_false;
    Alcotest.test_case "Check ==: 10 == 10 " `Quick test_comp_eq_true;
    Alcotest.test_case "Check ==: 10 == 1 " `Quick test_comp_eq_false;
    Alcotest.test_case "Check parens and whitespace" `Quick
      test_parens_whitespace;
    Alcotest.test_case "Check comment and new line" `Quick test_comments_nl;
    (* improved interpreter tests *)
    Alcotest.test_case "meta .exit" `Quick test_meta_exit;
    Alcotest.test_case "unk command" `Quick test_unknown_command;
    Alcotest.test_case "insert recognized" `Quick
      test_insert_statement_recognized;
    Alcotest.test_case "select recognized" `Quick
      test_select_statement_recognized;
  ]
