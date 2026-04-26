(** Holds variants for different types of nodes *)
type node_type =
  | Leaf
  | Internal
[@@deriving show]

type t = {
  mutable node_t : node_type;
  mutable parent : int;
  mutable cur_size : int;
  keys : Keys.value array;
  pointers : int array;
  (* max num of keys *)
  capacity : int;
  key_type : Keys.t;
}
[@@deriving show]
(** Data structure for nodes *)

val serialize_node : node_type -> Int32.t
val int32_to_node_t : Int32.t -> node_type
val print_leaf_node : t -> string
