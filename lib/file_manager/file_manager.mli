type t

val make : db_dirname:string -> block_size:int -> t
(** Init the file manager *)

val is_new : t -> bool
(** check if file manager is new *)

val get_blocksize : t -> int
(** Gets the blocksize of a filemanager in [int] *)

val get_file : t -> string -> Unix.file_descr
(** Return file in Unix description *)

val read : t -> Page.Block.t
(** Read block into the page *)
