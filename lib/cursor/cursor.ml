(*This will be what is used to do leaf searching and full-table scans *)
type t = {
  tree : Btree.t;
  mutable page_num : int;
  mutable cell_num : int;
  mutable end_of_table : bool;
}

let make ?(end_of_table = false) (tree : Btree.t) : t =
  { tree; page_num = tree.root_num; cell_num = 0; end_of_table }

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
  if cursor.cell_num >= node.cur_size then begin
    (* nah let's now go to the next leaf node *)
    (* let next_page_num = node.pointers.(cursor.cell_num) in *)
    let next_page_num = node.pointers.(node.capacity) in
    if next_page_num = Btree.unused_pointer_serial then
      cursor.end_of_table <- true
    else
      cursor.page_num <- next_page_num;
    cursor.cell_num <- 0
  end

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

(* Very similar to leaf node find function*)
(* Except, this should return a cursor to the leaf of this key*)
let rec internal_node_find (tree : Btree.t) (page_num : int) (key : Keys.value)
    : t =
  let rec binary_search (n : Nodes.t) (min_i : int) (max_i : int) : int =
    if min_i = max_i then
      min_i
    else
      let idx = (max_i + min_i) / 2 in
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
  | Nodes.Leaf -> leaf_node_find tree page_num key
  | Nodes.Internal -> begin
      let min_index = 0 in
      let one_past_max_idx = node.cur_size in

      (* damn did I screw that up*)
      let child_idx = binary_search node min_index one_past_max_idx in
      let child_page_num = node.pointers.(child_idx) in
      let child_node = Btree.get_node tree child_page_num in

      match child_node.node_t with
      | Nodes.Leaf -> leaf_node_find tree child_page_num key
      | Nodes.Internal -> internal_node_find tree child_page_num key
    end

let tree_find (tree : Btree.t) (key : Keys.value) : t =
  let root_node = Btree.get_node tree tree.root_num in
  match root_node.node_t with
  | Nodes.Leaf -> leaf_node_find tree tree.root_num key
  | Nodes.Internal -> internal_node_find tree tree.root_num key

let distribute_node_keys ~(old_node : Nodes.t) ~(new_node : Nodes.t)
    ~(insert_idx : int) ~(new_key : Keys.value) ~(new_pointer : int)
    ~(left_count : int) ~(total_entries : int) : unit =
  let get_entry (i : int) =
    if i = insert_idx then
      (new_key, new_pointer)
    else if i > insert_idx then
      (old_node.keys.(i - 1), old_node.pointers.(i - 1))
    else
      (old_node.keys.(i), old_node.pointers.(i))
  in

  let write_entry (n : Nodes.t) (idx : int) (key, pointer) =
    n.keys.(idx) <- key;
    n.pointers.(idx) <- pointer
  in

  let rec loop (i : int) =
    if i < 0 then
      ()
    else begin
      let entry = get_entry i in
      if i < left_count then
        write_entry old_node i entry
      else
        let new_node_idx = i - left_count in
        write_entry new_node new_node_idx entry;
        loop (i - 1)
    end
  in
  loop (total_entries - 1)

let leaf_node_split_and_insert (cursor : t) (key : Keys.value)
    (value_pointer : int) : unit =
  let tree = cursor.tree in
  let old_node = Btree.get_node tree cursor.page_num in
  let new_node = Btree.empty_node tree in
  let old_sib = old_node.pointers.(old_node.capacity) in

  (* this'll allow for an even split, getting the old capacity + 1 
     then splitting that in 2 and having the complement for the second on e*)
  let total_entries = old_node.capacity + 1 in
  let left_count = (total_entries + 1) / 2 in
  let right_count = total_entries - left_count in

  (* updating the cell count on both leaf nodes *)
  old_node.cur_size <- left_count;
  new_node.cur_size <- right_count;

  (* Now the idea is to divide the keys and new key evenly between old left and new right nodes.
     Starts from right*)
  (*awful *)
  distribute_node_keys ~old_node ~new_node ~insert_idx:cursor.cell_num
    ~new_key:key ~new_pointer:value_pointer ~left_count ~total_entries;

  (* means we need to split root*)
  if cursor.page_num = tree.root_num then begin
    let left_page_num = Btree.write_node_append tree old_node in
    let right_page_num = Btree.write_node_append tree new_node in

    (* update parents of old nodes *)
    old_node.parent <- tree.root_num;
    new_node.parent <- tree.root_num;

    old_node.pointers.(old_node.capacity) <- right_page_num;
    new_node.pointers.(new_node.capacity) <- old_sib;

    Btree.write_node tree old_node left_page_num;
    Btree.write_node tree new_node right_page_num;

    Btree.create_new_root tree ~left_child_page_num:left_page_num
      ~left_child:old_node ~right_child_page_num:right_page_num
  end
  else begin
    let new_page_num = Btree.write_node_append tree new_node in

    (* need to make sure the sibling point is still the end of each *)
    (*first old sibling *)
    new_node.pointers.(new_node.capacity) <-
      old_node.pointers.(old_node.capacity);
    old_node.pointers.(old_node.capacity) <- new_page_num;

    Btree.write_node tree old_node cursor.page_num;
    Btree.write_node tree new_node new_page_num
  end

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
  match node.node_t with
  | Nodes.Internal ->
      failwith "TRYING TO INSERT leaf_node_insert ON AN INTERNAL NODE"
  | Nodes.Leaf -> begin
      let num_cells = node.cur_size in
      (* if we hit the max and need to split a node*)
      if num_cells >= node.capacity then
        leaf_node_split_and_insert cursor key value_pointer
        (* else a regular split *)
      else begin
        if cursor.cell_num < num_cells then
          shift_cells cursor node num_cells;

        node.cur_size <- node.cur_size + 1;
        node.keys.(cursor.cell_num) <- key;
        node.pointers.(cursor.cell_num) <- value_pointer;

        Btree.write_node cursor.tree node cursor.page_num
      end
    end

let tree_start (tree : Btree.t) : t =
  (*cursor at key (id) 0: HACKY *)
  let cursor = tree_find tree (Keys.Integer (Int32.of_int 0)) in
  let node = Btree.get_node tree cursor.page_num in
  let num_cells = node.cur_size in
  let eot = num_cells = 0 in

  { tree; page_num = cursor.page_num; cell_num = 0; end_of_table = eot }

let collect_keys (tree : Btree.t) : int list =
  let cursor = tree_start tree in

  let rec collect acc =
    if cursor.end_of_table then
      List.rev acc
    else
      let node = Btree.get_node tree cursor.page_num in
      let key =
        match node.keys.(cursor.cell_num) with
        | Keys.Integer n -> Int32.to_int n
        | Keys.Varchar _ -> failwith "Only handles int keys atm"
      in
      cursor_advance cursor;
      collect (key :: acc)
  in
  collect []
