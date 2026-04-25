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

let serialize_node (node_t : node_type) : Int32.t =
  match node_t with
  | Leaf -> leaf_serial
  | Internal -> internal_serial

let int32_to_node_t (i32 : Int32.t) : node_type =
  if i32 = leaf_serial then
    Leaf
  else if i32 = internal_serial then
    Internal
  else
    failwith (Printf.sprintf "WRONG i32: %ld" i32)
