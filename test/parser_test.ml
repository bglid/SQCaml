open SQCaml

module To_test = struct
  let parse_int expected v =
    match v with
    | Ast.Int n -> Alcotest.(check int) "Same int value" expected n
    | _ -> Alcotest.fail "Expected [Int], got ???"

  let parse_meta_command expected v =
    match v with
    | Ast.META_COMMAND mc ->
        Alcotest.(check string) "Meta Command parsed" expected mc
    | Ast.COMMAND _ -> Alcotest.fail "Expected META_COMMAND got COMMAND"
    | _ -> Alcotest.fail "Expected [Ast.META_COMMAND]"

  let parse_command expected v =
    match v with
    | Ast.COMMAND c -> Alcotest.(check string) "Command parsed" expected c
    | Ast.META_COMMAND _ -> Alcotest.fail "Expected COMMAND got META_COMMAND"
    | _ -> Alcotest.fail "Expected [Ast.COMMAND], got other"
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

let () =
  Alcotest.run "SQCaml"
    [
      ( "parsing-tests",
        [
          Alcotest.test_case "Check parsed int" `Quick test_parsed_int;
          Alcotest.test_case "Check parsed META_COMMAND" `Quick
            test_parsed_meta_command;
          Alcotest.test_case "Check parsed COMMAND" `Quick test_parsed_command;
        ] );
    ]
