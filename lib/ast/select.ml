(* abstraction to make a module for preparing and executing select statement*)

type t = { fields : string list } [@@deriving show]

(* kinda stupid for now but will be better for future*)
let make (fields : string list) : t = { fields }

let execute_select (db : Db_session.t) (_ : t) : string =
  (* need to get the pages to deserialize *)
  (* let keys = Cursor.collect_keys in  *)
  let cursor = Cursor.tree_start db.index in
  (* let leaf = *)
  (*   Cursor.leaf_node_find db.index cursor.page_num *)
  (*     (Keys.Integer (Int32.of_int 20)) *)
  (* in *)
  let rec cursor_select (c : Cursor.t) (acc : string list) : string list =
    if c.end_of_table then
      List.rev acc
    else
      let row_page_num = Cursor.cursor_value cursor in

      let page =
        Storage_manager.get_block ~storage_m:db.storage_manager
          ~block_num:row_page_num
      in

      let row = Table.deserialize_from_page page in
      Cursor.cursor_advance c;
      let str_row =
        Int.to_string row.id ^ ", " ^ row.stop_name ^ ", " ^ row.rail_line
        ^ "\n"
      in
      cursor_select c (str_row :: acc)
  in
  let res = cursor_select cursor [] in
  List.fold_left (fun acc l -> acc ^ l) "" res
