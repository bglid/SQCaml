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
(** Takes in [node_t] node_type and serializes it to an Int32.t *)

val int32_to_node_t : Int32.t -> node_type
(** Takes in [i32] Int32.t and de-serializes it to node of node_type *)

val print_leaf_node : t -> string
(** Takes in a leaf node [n] and returns a string of it's Keys.value *)

val print_internal_node : t -> string
(** Takes in an internal node [n] and returns a string of it's Keys.value *)

val internal_node_child_pointer : t -> int -> int
(** Takes in an internal node [node] and an int [child_num] and returns a
    pointer to that child node *)

val internal_node_key : t -> int -> Keys.value
(** Returns the key of an internal node *)
