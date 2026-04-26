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

let test_page_int_mutate () =
  let page = Page.Page.make ~block_size:4096 in
  Page.Page.set_int32 page 0 Int32.one;
  let got = Page.Page.get_int32 page 0 in
  Alcotest.(check int32) "same int32 after roundtrip" Int32.one got

let test_file_manager_rw () =
  (* with_temp_dir @@ fun dirname -> *)
  let fm1 = File_manager.make ~db_dirname:"test.db" ~block_size:4096 in
  (* let file = Filename.concat dirname "test.db" in *)
  let page = Page.Page.make ~block_size:4096 in
  let block = Page.Block.make ~filename:"test.db" ~block_num:0 in
  Page.Page.set_int32 page 0 Int32.one;
  File_manager.write fm1 block page;
  File_manager.close fm1;

  let fm2 = File_manager.make ~db_dirname:"test.db" ~block_size:4096 in
  let newpage = Page.Page.make ~block_size:4096 in
  File_manager.read fm2 block newpage;

  Alcotest.(check int32)
    "page persisted after reopen" Int32.one
    (Page.Page.get_int32 newpage 0)

let tests =
  [
    Alcotest.test_case "Check page doesn't mutate Int32s" `Quick
      test_page_int_mutate;
    Alcotest.test_case "Check file manager can persist after reopen" `Quick
      test_file_manager_rw;
  ]
