(* abstraction to make a module for preparing and executing select statement*)
type t = { fields : string list } [@@deriving show]

(* kinda stupid for now but will be better for future*)
let make (fields : string list) : t = { fields }
let default_fields = [ "id"; "stop_name"; "rail_line" ]

let match_fields (res : string) (row : Table.row_t) =
  match res with
  | "id" -> Int.to_string row.id
  | "stop_name" -> row.stop_name
  | "rail_line" -> row.rail_line
  | _ -> failwith "Out of schema!!!"

let render_fields (fields : string list) row =
  fields
  |> List.map (fun field -> match_fields (String.lowercase_ascii field) row)
  |> String.concat ", "

let execute_select ?prepped_select (db : Db_session.t) : string =
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
      (* check this!!!!*)
      let row_page_num = Cursor.cursor_value c in

      let page =
        Storage_manager.get_block ~storage_m:db.storage_manager
          ~block_num:row_page_num
      in

      let row = Table.deserialize_from_page page in
      match prepped_select with
      | None -> begin
          Cursor.cursor_advance c;
          let str_row =
            Int.to_string row.id ^ ", " ^ row.stop_name ^ ", " ^ row.rail_line
            ^ "\n"
          in
          cursor_select c (str_row :: acc)
        end
      | Some prep -> begin
          Cursor.cursor_advance c;
          let str_row = render_fields prep.fields row ^ "\n" in
          cursor_select c (str_row :: acc)
        end
  in
  let res = cursor_select cursor [] in
  List.fold_left (fun acc l -> acc ^ l) "" res
