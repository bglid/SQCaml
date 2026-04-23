open Alcotest

let () =
  run "SQCaml"
    [
      ("interpreter", Interpreter_test.tests);
      (* ("table", Table_test.tests); *)
      (* ("pager", Pager_test.tests); *)
      (* ("btree", Btree_test.tests); *)
    ]
