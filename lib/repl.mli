(** Interactive SQCaml REPL for interacting with SQCaml, the B+ tree OCaml db *)

val start : Db_session.t -> unit
(** Takes Db_session.t [db] file to start the interactive SQCaml REPL *)
