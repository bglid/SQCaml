(** The Filemanager is how we parse through multiple files in our B+ tree
    storage implementation. *)

type t
(** Our file manager is how we interact with blocks on the disk *)

val make : db_dirname:string -> block_size:int -> t
(** Init the file manager *)

val is_new : t -> bool
(** check if file manager is new *)

val get_blocksize : t -> int
(** Gets the blocksize of a filemanager in [int] *)

val get_file : t -> string -> Unix.file_descr
(** Return file in Unix description *)

val read : t -> Page.Block.t -> Page.Page.t -> unit
(** Read block into the page *)

val write : t -> Page.Block.t -> Page.Page.t -> unit
(** Write block into the page *)

val size : t -> string -> int
(** return number of blocks in the file *)

val append : t -> string -> Page.Block.t
(** Append a block in a file and return the block id *)

val close : t -> unit
