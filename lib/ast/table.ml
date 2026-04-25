(* Starting with a minimal row struct *)
type row_t = {
  id : int;
  stop_name : string;
  rail_line : string;
}
[@@deriving show]

(* probably need to adjust this*)
type t = {
  table_id : int;
  table_name : string;
  table_fields : string list;
  rows : row_t list;
  num_rows : int;
}
[@@deriving show]

(* hard coding rows for serialization *)
let stop_name_size = 64
let rail_line_size = 64
let id_offset = 0
let stop_offset = id_offset + 4
let rail_offset = stop_name_size + stop_offset
let row_size = 4 + stop_name_size + rail_line_size
let make_row ~id ~stop_name ~rail_line : row_t = { id; stop_name; rail_line }

let create_table ~(id : int) ~(table_name : string) : t =
  let rows = [] in
  {
    table_id = id;
    table_name;
    table_fields = [];
    rows;
    num_rows = List.length rows;
  }

(* let open_table ~(table_name : string) : t = *)
(*   match table_name with *)
(*     |  *)

(* let already_in_table ~(table : t) ~(row : row_t) : bool = *)
(*   (* List.mem row table.rows *) *)
(*   let rec search (table_rows : row_t list) (id : int) : bool = *)
(*     match table_rows with *)
(*     | [] -> false *)
(*     | h :: tail -> *)
(*         if h.id = id then *)
(*           true *)
(*         else *)
(*           search tail id *)
(*   in *)
(*   search table.rows row.id *)

(* currently this is a bit functional in it's nature
   always returning a new table rather than adjusting the existing one.
   May want to consider changing rows to be mutable, or need to consider disk 
   writes here - TBD *)
(* let add_row ~(table : t) ~(row : row_t) : t = *)
(*   if already_in_table ~table ~row then *)
(*     table *)
(*   else *)
(*     (* let new_row_list = List.fold_left (fun acc l -> acc :: l) table.rows [row] in *) *)
(*     let new_row_list = table.rows @ [ row ] in *)
(*     { *)
(*       table_id = table.table_id; *)
(*       table_name = table.table_name; *)
(*       table_fields = table.table_fields; *)
(*       rows = new_row_list; *)
(*       num_rows = List.length new_row_list; *)
(*     } *)

let pad_fixed_size (size : int) (s : string) : string =
  if String.length s > size then
    failwith
      (Printf.sprintf "String too long.\nGreater than input size of %d" size)
  else
    s ^ String.make (size - String.length s) '\000'

let trim_null (s : string) : string =
  match String.index_opt s '\000' with
  | None -> s
  | Some thing -> String.sub s 0 thing

let serialize_to_page (row : row_t) ~(block_size : int) : Page.Page.t =
  if row_size > block_size then
    failwith "Row doesn't fit into page";

  let page = Page.Page.make ~block_size in
  (* serialize id, stop name, then rail name *)
  Page.Page.set_int32 page id_offset (Int32.of_int row.id);
  Page.Page.set_string_raw page stop_offset
    (pad_fixed_size stop_name_size row.stop_name);
  Page.Page.set_string_raw page rail_offset
    (pad_fixed_size rail_line_size row.rail_line);
  page

let deserialize_from_page (page : Page.Page.t) : row_t =
  (* get the id, stop name, and rail name from the Int32 offsets*)
  let id = Int32.to_int (Page.Page.get_int32 page id_offset) in
  let stop_name =
    trim_null (Page.Page.get_string_raw page stop_offset stop_name_size)
  in
  let rail_line =
    trim_null (Page.Page.get_string_raw page rail_offset rail_line_size)
  in
  { id; stop_name; rail_line }
