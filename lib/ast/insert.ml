(* abstraction to make a module for preparing and executing insert statement*)

type t = {
  (* tree : Btree.t; *)
  fields : string list;
  values : Constant.t list;
}
[@@deriving show]

(* HACKY for now, new table each time *)
(* let make (tablename : string) (fields : string list) (values : Constant.t list) *)
let make (fields : string list) (values : Constant.t list) : t =
  (* { table = Table.create_table ~id:777 ~table_name:tablename; fields; values } *)
  { fields; values }

let execute_insert (db : Db_session.t) (preped_insert : t) : string =
  (* serialize the insert to a page*)
  let new_row =
    match preped_insert.values with
    | [ id; stop; rail ] ->
        Table.make_row
          ~id:(Int32.to_int (Constant.to_int id))
          ~stop_name:(Constant.to_str stop) ~rail_line:(Constant.to_str rail)
    | _ -> failwith "Wrong amount of args passed"
  in

  (* getting cursor based on row id*)
  let key_to_insert = Keys.Integer (Int32.of_int new_row.id) in
  let cursor = Cursor.tree_find db.index key_to_insert in
  let leaf = Btree.get_node db.index cursor.page_num in

  if
    cursor.cell_num < leaf.cur_size
    && Keys.equals leaf.keys.(cursor.cell_num) key_to_insert
  then
    "duplicate key"
    (* split the node!*)
  else
    (* serialize the page and write to a block! *)
    let page =
      Table.serialize_to_page new_row
        ~block_size:(File_manager.get_blocksize db.file_manager)
    in
    let row_block =
      Storage_manager.append ~storage_m:db.storage_manager ~page
    in
    (* block num where we want to write to disk!!*)
    let row_block_num = Page.Block.block_num row_block in
    let key = Keys.Integer (Int32.of_int new_row.id) in
    (* getting and writing our tree node to disk FINALLY*)
    Cursor.leaf_node_insert cursor key row_block_num;

    (* printing the res *)
    let res = "Inserted " ^ new_row.stop_name ^ " " ^ new_row.rail_line ^ " " in
    res
