(* abstraction to make a module for preparing and executing insert statement*)

type t = {
  (* table : Table.t; *)
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
  let page =
    Table.serialize_to_page new_row
      ~block_size:(File_manager.get_blocksize db.file_manager)
  in
  (* write the page *)
  let row_block_num =
    Storage_manager.append ~storage_m:db.storage_manager ~page
  in
  File_manager.write db.file_manager row_block_num page;

  (* new_fields *)
  let res = "Inserted " ^ new_row.stop_name ^ " " ^ new_row.rail_line ^ " " in
  res
