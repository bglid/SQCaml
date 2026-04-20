type node_type =
  | Leaf
  | Internal
[@@deriving show]

[@@@warning "-69"]

type t = {
  (* There are 2 node types, leaf or internal *)
  mutable node_type : node_type;
  mutable parent : int;
  mutable num_keys : int;
  keys : Keys.value array;
  pointers : int array;
  (* max num of keys *)
  capacity : int;
  key_type : Keys.t;
}
[@@deriving show]

[@@@warning "+69"]
