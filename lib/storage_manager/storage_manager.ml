open File_manager

(* Block 0 is for the freelist. If it's not in the freelist it's being used *)
(* NOTE: The 'head_page', is top of the freelist *)

type t = {
  file_manager : File_manager.t;
  storage_file : string;
  mutable head_page : Page.Page.t (* First page contains metadata *);
}

let get_head_page ~storage_manager = storage_manager.head_page

let set_head_page ~storage_manager (page : Page.Page.t) =
  storage_manager.head_page <- page;
  let block_id =
    Page.Block.make ~filename:storage_manager.storage_file ~block_num:0
  in
  File_manager.write storage_manager.file_manager block_id
    storage_manager.head_page

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

let append ~(storage_m : t) ~(page : Page.Page.t) : Page.Block.t =
  let file_m = storage_m.file_manager in
  let block_size = File_manager.get_blocksize file_m in
  let sfile = storage_m.storage_file in
  let head_page = storage_m.head_page in
  let head_ptr = Page.Block.make ~filename:sfile ~block_num:0 in
  let next_ptr = Int32.to_int (Page.Page.get_int32 head_page 0) in
  (* If next = 0; then free list is empty so we can just append at end *)
  if next_ptr = 0 then (
    (* let blocksize = File_manager.get_blocksize file_m in *)
    let block = File_manager.append file_m sfile in
    File_manager.write file_m block page;
    block (* else get a block from the free list *))
  else
    (* read from freelist into the page *)
    let next_ptr = Page.Block.make ~filename:sfile ~block_num:next_ptr in
    let next_page = Page.Page.make ~block_size in
    File_manager.read file_m next_ptr next_page;
    (* save from first element and update head position *)
    let next_next_ptr = Page.Page.get_int32 next_page 0 in
    Page.Page.set_int32 head_page 0 next_next_ptr;
    File_manager.write file_m head_ptr head_page;
    (* write appended data from freelist*)
    File_manager.write file_m next_ptr page;
    next_ptr

let delete ~(storage_m : t) ~(block : Page.Block.t) : unit =
  let file_m = storage_m.file_manager in
  let block_size = File_manager.get_blocksize file_m in
  let sfile = storage_m.storage_file in
  let head_page = storage_m.head_page in
  let head_ptr = Page.Block.make ~filename:sfile ~block_num:0 in
  let next_ptr = Page.Page.get_int32 head_page 0 in
  let page = Page.Page.make ~block_size in
  Page.Page.set_int32 page 0 next_ptr;
  Page.Page.set_int32 head_page 0 (Int32.of_int (Page.Block.block_num block));
  File_manager.write file_m head_ptr head_page;
  File_manager.write file_m block page

let update ~(storage_m : t) ~(block : Page.Block.t) ~(page : Page.Page.t) : unit
    =
  let file_m = storage_m.file_manager in
  File_manager.write file_m block page

let update_block_num ~(storage_m : t) ~(block_num : int) ~(page : Page.Page.t) :
    unit =
  let block = Page.Block.make ~filename:storage_m.storage_file ~block_num in
  update ~storage_m ~block ~page

let get_block ~(storage_m : t) ~(block_num : int) : Page.Page.t =
  let file_m = storage_m.file_manager in
  let block_size = File_manager.get_blocksize file_m in
  let page = Page.Page.make ~block_size in
  let block = Page.Block.make ~filename:storage_m.storage_file ~block_num in
  File_manager.read file_m block page;
  page
