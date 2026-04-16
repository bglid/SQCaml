open File_manager

type t = {
  file_manager : File_manager.t;
  storage_file : string;
  mutable head_page : Page.Page.t (* First page contains metadata *);
}

let make ~file_manager ~storage_file =
  let block_size = File_manager.get_blocksize file_manager in
  let head_page = Page.Page.make ~block_size in
  let block = Page.Block.make ~filename:storage_file ~block_num:0 in
  if File_manager.size file_manager storage_file = 0 then (
    Page.Page.set_int32 head_page 0 (Int32.of_int 0);
    File_manager.write file_manager block head_page)
  else
    File_manager.read file_manager block head_page;
  { file_manager; storage_file; head_page }

(* val append : t -> Page.Page.t -> Page.Block.t *)
(* (**  *) *)
(**)
(* val delete : t -> Page.Block.t -> unit *)
(* (** *) *)
(**)
(* val update : t -> Page.Block.t -> Page.Page.t -> unit *)
(* val update_block_num : t -> int -> Page.Page.t -> unit *)
(* val get_block : t -> int -> Page.Page.t *)
