type node_type =
  | Leaf
  | Internal
[@@deriving show]

[@@@warning "-69"]

type t = {
  (* There are 2 node types, leaf or internal *)
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

[@@@warning "+69"]

(* Constants for serialization*)
let leaf_serial = Int32.of_int 2863311530 (* 0xAAAAAAAA *)
let internal_serial = Int32.of_int 3149642683
(* 0xBBBBBBBB *)
(* let unused_pointer_serial = 3722304989 (*0xDDDDDDDD *) *)

let serialize_node (node_t : node_type) : Int32.t =
  match node_t with
  | Leaf -> leaf_serial
  | Internal -> internal_serial
