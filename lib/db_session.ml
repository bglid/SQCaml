type t = {
  file_manager : File_manager.t;
  storage_manager : Storage_manager.t;
  mutable index : Btree.t;
}

let block_size = 4096
let storage_file = "sqcaml.db"

let open_db (db_dir : string) : t =
  (* make filemanager *)
  let fm = File_manager.make ~db_dirname:db_dir ~block_size in

  (* make storage manager*)
  let sm = Storage_manager.make ~file_manager:fm ~storage_file in

  (* make btree, for simplicity starting with one key type *)
  let btree = Btree.open_btree sm Keys.TInteger in

  { file_manager = fm; storage_manager = sm; index = btree }

let close_db (db : t) : unit = File_manager.close db.file_manager
