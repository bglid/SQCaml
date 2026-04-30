(* struct/module for pointing to rows in a table *)

(*This will be what is used to do leaf searching and full-table scans *)
type t = {
  tree : Btree.t;
  mutable page_num : int;
  mutable cell_num : int;
  mutable end_of_table : bool;
}

val make : ?end_of_table:bool -> Btree.t -> t
(** Takes in option [end_of_table] and btree [tree] to create a cursor *)

val cursor_value : t -> int
(** Takes in a cursor and returns the value at the leaf node it's pointing at *)

val cursor_advance : t -> unit
(** Takes in a cursor and advances the cell it's pointing at *)

val leaf_node_find : Btree.t -> int -> Keys.value -> t
(** Takes in a Btree [tree] [page_num]int and [key] key value to return a cursor
    pointing at the leaf directed by the key and page num *)

val internal_node_find : Btree.t -> int -> Keys.value -> t
(** At a internal node: takes in a Btree, [tree] [page_num]int and [key] key
    value to return a cursor pointing at the leaf directed by the key and page
    num *)

val tree_find : Btree.t -> Keys.value -> t
(** Takes btree [tree] and key_val [key] returns the position of the cursor at
    said [key]*)

val distribute_node_keys :
  old_node:Nodes.t ->
  new_node:Nodes.t ->
  insert_idx:int ->
  new_key:Keys.value ->
  new_pointer:int ->
  left_count:int ->
  total_entries:int ->
  unit
(** Takes in an oldnode and new node upon a split and distributes nodes and
    pointers for the split *)

val leaf_node_split_and_insert : t -> Keys.value -> int -> unit
(** Takes in a cursor and key value upon a split and creates a new node and
    moves half the cells and update parent *)

val leaf_node_insert : t -> Keys.value -> int -> unit
(** Takes in a cursor, key_value, and pointer and inserts into a leaf node *)

val tree_start : Btree.t -> t
(** Takes in a btree [tree] and returns a cursor pointed at the start (left-most
    side) of the tree *)

val collect_keys : Btree.t -> int list
(** Takes in a btree [tree] and moves the cursor along, adding the keys to an
    int list to return *)
