(** This is what our B+ tree will interact with to interact with the file
    manager *)

type t

val make : File_manager.t -> string -> t
(** Takes a file manager and a string to create a new storage manager *)

val append : t -> Page.Page.t -> Page.Block.t
(** *)

(* val delete : t -> Page.Block.t -> unit *)
(** *)

val update : t -> Page.Block.t -> Page.Page.t -> unit
val update_block_num : t -> int -> Page.Page.t -> unit
val get_block : t -> int -> Page.Page.t
