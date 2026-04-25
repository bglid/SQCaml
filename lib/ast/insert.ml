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
  let cursor = Cursor.tree_end db.index in

  (* check to guard capacity*)
  let leaf = Btree.get_node db.index cursor.page_num in
  if leaf.cur_size >= leaf.capacity then
    failwith "HELP! please implement splitting plz."
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
