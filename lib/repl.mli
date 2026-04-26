(** Interactive SQCaml REPL for interacting with SQCaml, the B+ tree OCaml db *)

(* module Command : sig *)
(*   type t *)
(* end *)

(* val check_meta_command : unit -> unit *)
(** Checks for meta command and returns Unrecognized command if not recognized
*)

val start : Db_session.t -> unit
(** Start the interactive SQCaml REPL *)
