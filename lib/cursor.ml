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

let tree_start (tree : Btree.t) : t =
  let root_node = Btree.get_node tree tree.root_num in
  let num_cells = root_node.cur_size in
  let eot = num_cells = 0 in

  { tree; page_num = tree.root_num; cell_num = 0; end_of_table = eot }

(* let tree_end (tree : Btree.t) : t = *)
(*   let root_node = Btree.get_node tree tree.root_num in *)
(*   let num_cells = root_node.cur_size in *)
(*   let cell_num = num_cells in *)
(*   { tree; page_num = tree.root_num; cell_num; end_of_table = true } *)

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

(* finally, leaf node insert*)
let leaf_node_insert (cursor : t) (key : Keys.value) (value_pointer : int) :
    unit =
  let rec shift_cells (c : t) (n : Nodes.t) (i : int) =
    if i <= c.cell_num then
      ()
    else begin
      n.keys.(i) <- n.keys.(i - 1);
      n.pointers.(i) <- n.pointers.(i - 1);
      shift_cells c n (i - 1)
    end
  in

  (* get the node where the cursor is pointing*)
  let node = Btree.get_node cursor.tree cursor.page_num in
  let num_cells = node.cur_size in
  if num_cells >= node.capacity then
    failwith "Help! please implement splitting!!!!";

  if cursor.cell_num < num_cells then
    shift_cells cursor node num_cells;

  node.cur_size <- node.cur_size + 1;
  node.keys.(cursor.cell_num) <- key;
  node.pointers.(cursor.cell_num) <- value_pointer;

  Btree.write_node cursor.tree node cursor.page_num

(*basically just does b-search and returns cursor with some guards *)
let leaf_node_find (tree : Btree.t) (page_num : int) (key : Keys.value) : t =
  (* binary search for id idx *)
  let rec binary_search (n : Nodes.t) (min_i : int) (max_i : int) : int =
    if min_i = max_i then
      min_i
    else
      let idx = (min_i + max_i) / 2 in
      let key_idx = n.keys.(idx) in
      if Keys.equals key key_idx then
        idx
      else if Keys.less_than key key_idx then
        let max_i = idx in
        binary_search n min_i max_i
      else
        let min_i = idx + 1 in
        binary_search n min_i max_i
  in

  let node = Btree.get_node tree page_num in

  match node.node_t with
  | Nodes.Internal -> failwith "Can't leaf search on an internal node"
  | Nodes.Leaf -> begin
      let min_index = 0 in
      let one_past_max_idx = node.cur_size in

      let cell_num = binary_search node min_index one_past_max_idx in
      { tree; page_num; cell_num; end_of_table = cell_num = node.cur_size }
    end

(* returns the position of the cursor at said key*)
let tree_find (tree : Btree.t) (key : Keys.value) : t =
  let root_node = Btree.get_node tree tree.root_num in
  match root_node.node_t with
  | Nodes.Leaf -> leaf_node_find tree tree.root_num key
  | Nodes.Internal -> failwith "Not implemented yet"
