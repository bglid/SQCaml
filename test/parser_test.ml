open SQCaml

module To_test = struct
  let parse_int expected v =
    match v with
    | Ast.Int n -> Alcotest.(check int) "Same int value" expected n
    | _ -> Alcotest.fail "Expected [Int], got ???"

  let parse_meta_command expected v =
    match v with
    | Ast.Meta_Command mc ->
        Alcotest.(check string) "Meta Command parsed" expected mc
    | Ast.Command _ -> Alcotest.fail "Expected Meta_COMMAND got COMMAND"
    | _ -> Alcotest.fail "Expected [Ast.Meta_Command]"

  let parse_command expected v =
    match v with
    | Ast.Command c -> Alcotest.(check string) "Command parsed" expected c
    | Ast.Meta_Command _ -> Alcotest.fail "Expected COMMAND got META_COMMAND"
    | _ -> Alcotest.fail "Expected [Ast.Command], got other"

  let run_interpreter expected v =
    Alcotest.(check string) "Interpreter test" expected v
end

let test_parsed_int () =
  let expr = Interpreter.parse "22" in
  To_test.parse_int 22 expr

let test_parsed_meta_command () =
  let expr = Interpreter.parse ".exit" in
  To_test.parse_meta_command ".exit" expr

let test_parsed_command () =
  let expr = Interpreter.parse "exit" in
  To_test.parse_command "exit" expr

let test_interpreted_int () =
  let expr = Interpreter.interpreta "22" in
  To_test.run_interpreter (string_of_int 22) expr

(* Int tests *)
let test_add_int () =
  let expr = Interpreter.interpreta "22 + 20" in
  To_test.run_interpreter (string_of_int 42) expr

let test_mult_int () =
  let expr = Interpreter.interpreta "2 * 20" in
  To_test.run_interpreter (string_of_int 40) expr

let test_sub_int () =
  let expr = Interpreter.interpreta "22 - 20" in
  To_test.run_interpreter (string_of_int 2) expr

let test_div_int () =
  let expr = Interpreter.interpreta "20 / 2" in
  To_test.run_interpreter (string_of_int 10) expr

let test_precedence_mult_int () =
  let expr = Interpreter.interpreta "2 * 3 * 10" in
  To_test.run_interpreter (string_of_int 60) expr

let test_precedence_mult_add_int () =
  let expr = Interpreter.interpreta "2 * 3 + 10" in
  To_test.run_interpreter (string_of_int 16) expr

let test_precedence_add_mult_int () =
  let expr = Interpreter.interpreta "2 + 3 * 10" in
  To_test.run_interpreter (string_of_int 32) expr

(* Float tests *)
let test_add_float () =
  let expr = Interpreter.interpreta "22.1 + 20.0" in
  To_test.run_interpreter (string_of_float 42.1) expr

let test_mult_float () =
  let expr = Interpreter.interpreta "2. * 20." in
  To_test.run_interpreter (string_of_float 40.) expr

let test_sub_float () =
  let expr = Interpreter.interpreta "22. - 20." in
  To_test.run_interpreter (string_of_float 2.) expr

let test_div_float () =
  let expr = Interpreter.interpreta "20. / 2." in
  To_test.run_interpreter (string_of_float 10.) expr

(* Bools *)
let test_true () =
  let expr = Interpreter.interpreta "TRUE" in
  To_test.run_interpreter (string_of_bool true) expr

let test_false () =
  let expr = Interpreter.interpreta "FALSE" in
  To_test.run_interpreter (string_of_bool false) expr

(* certainty with interpreter *)

let test_parens_whitespace () =
  let expr = Interpreter.interpreta "(1 + 1) * (2 + 2)" in
  To_test.run_interpreter (string_of_int 8) expr

let test_comments_nl () =
  let expr = Interpreter.interpreta "--test  \n  (2 + 2)" in
  To_test.run_interpreter (string_of_int 4) expr

let () =
  Alcotest.run "SQCaml"
    [
      ( "parsing-tests",
        [
          Alcotest.test_case "Check parsed int" `Quick test_parsed_int;
          Alcotest.test_case "Check parsed Meta_Command" `Quick
            test_parsed_meta_command;
          Alcotest.test_case "Check parsed Command" `Quick test_parsed_command;
        ] );
      ( "Interpreter int tests",
        [
          Alcotest.test_case "Check interpreted '22' int" `Quick
            test_interpreted_int;
          Alcotest.test_case "Check intpreted add math '22 + 20'" `Quick
            test_add_int;
          Alcotest.test_case "Check intpreted mult math '2 * 20'" `Quick
            test_mult_int;
          Alcotest.test_case "Check intpreted sub math " `Quick test_sub_int;
          Alcotest.test_case "Check intpreted div math " `Quick test_div_int;
          Alcotest.test_case "Check intpreted precedence mult mult '2 * 3 * 20'"
            `Quick test_precedence_mult_int;
          Alcotest.test_case "Check intpreted precedence mult add '2 * 3 + 10'"
            `Quick test_precedence_mult_add_int;
          Alcotest.test_case "Check intpreted precedence add mult '2 + 3 * 10'"
            `Quick test_precedence_add_mult_int;
        ] );
      ( "Interpreter float tests",
        [
          Alcotest.test_case "Check intpreted add float " `Quick test_add_float;
          Alcotest.test_case "Check intpreted mult float math " `Quick
            test_mult_float;
          Alcotest.test_case "Check intpreted sub float math " `Quick
            test_sub_float;
          Alcotest.test_case "Check intpreted div float math " `Quick
            test_div_float;
        ] );
      ( "Interpreter bool tests",
        [
          Alcotest.test_case "Check TRUE" `Quick test_true;
          Alcotest.test_case "Check FALSE" `Quick test_false;
        ] );
      ( "Interpreter functionality tests",
        [
          Alcotest.test_case "Check parens and whitespace" `Quick
            test_parens_whitespace;
          Alcotest.test_case "Check comment and new line" `Quick
            test_comments_nl;
        ] );
    ]
