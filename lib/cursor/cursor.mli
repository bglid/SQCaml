(* struct/module for pointing to rows in a table *)

(*This will be what is used to do leaf searching and full-table scans *)
type t = {
  tree : Btree.t;
  mutable page_num : int;
  mutable cell_num : int;
  mutable end_of_table : bool;
}

val make : ?end_of_table:bool -> Btree.t -> t
val cursor_value : t -> int
val cursor_advance : t -> unit
val tree_start : Btree.t -> t
val collect_keys : Btree.t -> int list
val leaf_node_find : Btree.t -> int -> Keys.value -> t
val internal_node_find : Btree.t -> int -> Keys.value -> t
val tree_find : Btree.t -> Keys.value -> t

val distribute_node_keys :
  old_node:Nodes.t ->
  new_node:Nodes.t ->
  insert_idx:int ->
  new_key:Keys.value ->
  new_pointer:int ->
  left_count:int ->
  total_entries:int ->
  unit

val leaf_node_split_and_insert : t -> Keys.value -> int -> unit
val leaf_node_insert : t -> Keys.value -> int -> unit
