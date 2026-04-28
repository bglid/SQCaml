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

let print_leaf_node (n : t) : string =
  if n.node_t <> Leaf then
    "Not a leaf node"
  else
    let lines =
      List.init n.cur_size (fun i ->
          match n.keys.(i) with
          | Keys.Integer n -> Printf.sprintf "- %d\n" (Int32.to_int n)
          | Keys.Varchar v -> Printf.sprintf "- %s\n" v)
    in
    Printf.sprintf "leaf (size %d)\n%s" n.cur_size (String.concat "" lines)

let print_internal_node (n : t) : string =
  if n.node_t <> Internal then
    "Not an internal node"
  else
    Printf.sprintf "Internal (size %d)\n" n.cur_size

let internal_node_child_pointer (node : t) (child_num : int) : int =
  match node.node_t with
  | Leaf -> failwith "This is a leafnode!"
  | Internal ->
      let num_keys = node.cur_size in
      if child_num > num_keys then
        failwith "Tried to access child num that's greater than num of keys!"
      else if child_num = num_keys then
        node.pointers.(num_keys)
      else
        node.pointers.(child_num)

let internal_node_key (node : t) (key_num : int) : Keys.value =
  match node.node_t with
  | Leaf -> failwith "This is a leafnode!"
  | Internal -> node.keys.(key_num)
