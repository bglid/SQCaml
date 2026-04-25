(* struct/module for pointing to rows in a table *)

(*This will be what is used to do leaf searching and full-table scans *)
type t = {
  tree : Btree.t;
  mutable page_num : int;
  mutable cell_num : int;
  mutable end_of_table : bool;
}

(* let make ?(end_of_table = false) (table : Table.t) : t = *)
let make ?(end_of_table = false) (tree : Btree.t) : t =
  { tree; page_num = tree.root_num; cell_num = 0; end_of_table }

(* let table_end (table : Table.t) : t = *)
(*   let new_cursor = *)
(*     make ?row_num:(Some table.num_rows) ?end_of_table:(Some true) table *)
(*   in *)
(*   new_cursor *)

let tree_start (tree : Btree.t) : t =
  let root_node = Btree.get_node tree tree.root_num in
  let num_cells = root_node.cur_size in
  let eot = num_cells = 0 in

  { tree; page_num = tree.root_num; cell_num = 0; end_of_table = eot }

let tree_end (tree : Btree.t) : t =
  let root_node = Btree.get_node tree tree.root_num in
  let num_cells = root_node.cur_size in
  let cell_num = num_cells in
  { tree; page_num = tree.root_num; cell_num; end_of_table = true }

(* get the val at a leaf node *)
let cursor_value (cursor : t) : int =
  let node = Btree.get_node cursor.tree cursor.page_num in
  match node.node_t with
  | Nodes.Internal -> failwith "Cursor err: pointing to internal node"
  | Nodes.Leaf ->
      if cursor.cell_num >= node.cur_size then
        failwith "Cursor err: Cell number is oob";

      node.pointers.(cursor.cell_num)

let cursor_advance (cursor : t) : unit =
  let node = Btree.get_node cursor.tree cursor.page_num in
  cursor.cell_num <- cursor.cell_num + 1;
  if cursor.cell_num >= node.cur_size then
    cursor.end_of_table <- true
