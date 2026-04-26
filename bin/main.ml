let () =
  Printf.printf "*********SQCaml*********\n\r";
  let db_dir =
    if Array.length Sys.argv >= 2 then
      Sys.argv.(1)
    else
      "sqcaml.db"
  in
  let db = SQCaml.Db_session.open_db db_dir in
  try SQCaml.Repl.start db
  with exn ->
    SQCaml.Db_session.close_db db;
    raise exn
