type t = {
  is_new : bool;
  db_dir : Unix.dir_handle;
  db_dirname : string;
  block_size : int;
  open_files : (string, Unix.file_descr) Hashtbl.t;
}

exception InitDBError
(* exception FileMgrReadErr *)

(* helper for clearning temp directories *)
let rec clean_temp_dir db_dirname db_dir =
  try
    let curr_file = Unix.readdir db_dir in
    if String.length curr_file >= 4 && String.sub curr_file 0 4 = "temp" then (
      Sys.remove (Filename.concat db_dirname curr_file);
      clean_temp_dir db_dirname db_dir)
    else
      clean_temp_dir db_dirname db_dir
  with End_of_file -> ()

(* Constructor for File Manager *)
let make ~db_dirname ~block_size : t =
  let db_dir, is_new =
    try
      let stat = Unix.stat db_dirname in
      (* information of the file *)
      if stat.st_kind = Unix.S_DIR then
        (Unix.opendir db_dirname, false)
      else
        raise InitDBError
    with
    (* create file handler if DB directory doesn't exist *)
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        let _ = Unix.mkdir db_dirname 0o755 in
        (Unix.opendir db_dirname, true)
    | _ -> raise InitDBError
  in
  clean_temp_dir db_dirname db_dir;
  Unix.rewinddir db_dir;
  let open_files = Hashtbl.create 10 in
  { is_new; db_dir; db_dirname; block_size; open_files }

let is_new (file_mger : t) : bool = file_mger.is_new
let get_blocksize (file_mger : t) : int = file_mger.block_size

let get_file (file_mger : t) (filename : string) : Unix.file_descr =
  match Hashtbl.find_opt file_mger.open_files filename with
  | Some file_desc -> file_desc
  | None ->
      let full_path = Filename.concat file_mger.db_dirname filename in
      let file_desc =
        Unix.openfile full_path Unix.[ O_RDWR; O_CREAT; O_SYNC ] 0o755
      in
      Hashtbl.add file_mger.open_files filename file_desc;
      file_desc

let read (file_mger : t) (block : Page.Block.t) (page : Page.Page.t) : unit =
  let file_desc = get_file file_mger (Page.Block.file_name block) in
  let offset = Page.Block.block_num block * file_mger.block_size in
  let _ = Unix.lseek file_desc offset SEEK_SET in
  let n =
    Unix.read file_desc (Page.Page.contents page) 0 file_mger.block_size
  in
  if n = 0 then
    Page.Page.zero_out page
  else
    ()

(* Repeatedly calls write until all bytes are written, Unix.write doesn't guarantee this *)
let rec write_n file_desc page offset n : unit =
  if n = 0 then
    ()
  else
    let bytes_written = Unix.write file_desc page offset n in
    write_n file_desc page (offset + bytes_written) (n - bytes_written)

let write (file_mger : t) (block : Page.Block.t) (page : Page.Page.t) : unit =
  let file_desc = get_file file_mger (Page.Block.file_name block) in
  let offset = Page.Block.block_num block * file_mger.block_size in
  let _ = Unix.lseek file_desc offset SEEK_SET in
  write_n file_desc (Page.Page.contents page) 0 file_mger.block_size

(* val size : t -> string -> int *)
let size (file_mger : t) (filename : string) : int =
  let _ = get_file file_mger filename in
  let full_path = Filename.concat file_mger.db_dirname filename in
  let stat = Unix.stat full_path in
  (* Gives back the size in bytes*)
  stat.st_size / file_mger.block_size

let append (file_mger : t) (filename : string) : Page.Block.t =
  let block_num = size file_mger filename in
  let block = Page.Block.make ~filename ~block_num in
  let b = Bytes.make file_mger.block_size '\000' in
  let file_desc = get_file file_mger filename in
  let _ = Unix.lseek file_desc (block_num * file_mger.block_size) SEEK_SET in
  write_n file_desc b 0 file_mger.block_size;
  block

let close (file_mger : t) : unit =
  Hashtbl.iter
    (fun _ files ->
      (try Unix.fsync files with _ -> ());
      Unix.close files)
    file_mger.open_files;
  Hashtbl.clear file_mger.open_files;
  Unix.closedir file_mger.db_dir
