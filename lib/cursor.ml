(* struct/module for pointing to rows in a table *)

(*This will be what is used to do leaf searching and full-table scans *)
type t = {
  tree : Btree.t;
  mutable page_num : int;
  mutable cell_num : int;
  mutable end_of_table : bool;
}

(* let make ?(end_of_table = false) (table : Table.t) : t = *)
(*   { tree = ; page_num = Table.page_num; end_of_table } *)
(**)
(* let table_start (table : Table.t) : t = *)
(*   let new_cursor = make ?end_of_table:(Some (table.num_rows = 0)) table in *)
(*   new_cursor *)
(**)
(* let table_end (table : Table.t) : t = *)
(*   let new_cursor = *)
(*     make ?row_num:(Some table.num_rows) ?end_of_table:(Some true) table *)
(*   in *)
(*   new_cursor *)
