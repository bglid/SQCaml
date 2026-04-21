(** This is what our B+ tree will interact with to interact with the file
    manager *)

type t = {
  file_manager : File_manager.t;
  storage_file : string;
  mutable head_page : Page.Page.t (* First page contains metadata *);
}

val make : file_manager:File_manager.t -> storage_file:string -> t
(** Takes a file manager and a string to create a new storage manager *)

val get_head_page : storage_manager:t -> Page.Page.t
(** Get head page from storage manager [t] *)

val set_head_page : storage_manager:t -> Page.Page.t -> unit
(** Read head page from storage manager and write new block to it *)

val append : storage_m:t -> page:Page.Page.t -> Page.Block.t
(** Append new block to [page] if the next item in the freelist is 0. Else read
    in a new block into the page from the freelist *)

val delete : storage_m:t -> block:Page.Block.t -> unit
(** Write 0 into the block *)

val update : storage_m:t -> block:Page.Block.t -> page:Page.Page.t -> unit
(** Update value at block given page *)

val update_block_num : storage_m:t -> block_num:int -> page:Page.Page.t -> unit
(** Update block number *)

val get_block : storage_m:t -> block_num:int -> Page.Page.t
(** Read block given block number as a page *)
