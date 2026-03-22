open SQCaml

module To_test = struct
  let parse_int expected v =
    match v with
    | Ast.Int n -> Alcotest.(check int) "Same int value" expected n
    | _ -> Alcotest.fail "Expected [Int], got ???"
end

let test_parsed_int () =
  let expr = Interpreter.parse "22" in
  To_test.parse_int 22 expr

let () =
  Alcotest.run "SQCaml"
    [
      ( "parsed-int-test",
        [ Alcotest.test_case "Check parsed int" `Quick test_parsed_int ] );
    ]
