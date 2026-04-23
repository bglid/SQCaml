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
  rows : row_t list;
}
[@@deriving show]

let make_row ~id ~stop_name ~rail_line : row_t = { id; stop_name; rail_line }

let create_table ~(id : int) ~(table_name : string) : t =
  { table_id = id; table_name; rows = [] }

(* let open_table ~(id : int) : t = *)

let already_in_table ~(table : t) ~(row : row_t) : bool =
  (* List.mem row table.rows *)
  let rec search (table_rows : row_t list) (id : int) : bool =
    match table_rows with
    | [] -> false
    | h :: tail ->
        if h.id = id then
          true
        else
          search tail id
  in
  search table.rows row.id

(* currently this is a bit functional in it's nature
   always returning a new table rather than adjusting the existing one.
   May want to consider changing rows to be mutable, or need to consider disk 
   writes here - TBD *)
let add_row ~(table : t) ~(row : row_t) : t =
  if already_in_table ~table ~row then
    table
  else
    (* let new_row_list = List.fold_left (fun acc l -> acc :: l) table.rows [row] in *)
    let new_row_list = table.rows @ [ row ] in
    {
      table_id = table.table_id;
      table_name = table.table_name;
      rows = new_row_list;
    }
