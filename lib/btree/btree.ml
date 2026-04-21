[@@@warning "-69"]

(* minimal B+ tree struct *)
type t = {
  storage_m : Storage_manager.t; (* file used to store to disk *)
  key : Keys.t;
  mutable root : Nodes.t;
  mutable root_num : int;
}

[@@@warning "+69"]

let serialize (node : Nodes.t) (block_size : int) : Page.Page.t =
  (* recursive iterative helpers*)
  let rec layout_keys (nd : Nodes.t) (pg : Page.Page.t) pair_size i : unit =
    if i >= nd.capacity then
      ()
    else
      let key_offset = 12 + (i * pair_size) + 4 in
      begin match nd.keys.(i) with
      | Integer n -> Page.Page.set_int32 pg key_offset n
      | Varchar s -> Page.Page.set_string_raw pg key_offset s
      end;
      layout_keys nd pg pair_size (i + 1)
  in

  let rec layout_pointers (nd : Nodes.t) (pg : Page.Page.t) pair_size i : unit =
    if i > nd.capacity then
      ()
    else
      let pointer_offset = 12 + (i * pair_size) in
      Page.Page.set_int32 pg pointer_offset (Int32.of_int nd.pointers.(i));
      layout_pointers nd pg pair_size (i + 1)
  in

  (* Acutal serialization below*)
  let page = Page.Page.make ~block_size in
  (* set node type at first 4 bytes *)
  Page.Page.set_int32 page 0 (Nodes.serialize_node node.node_t);
  (* Set parent offset at 4 *)
  Page.Page.set_int32 page 4 (Int32.of_int node.parent);
  (* set curr size at 8 *)
  Page.Page.set_int32 page 8 (Int32.of_int node.cur_size);
  (* get the pointer -> key size, 4 bytes pointer + M bytes key*)
  let pair_size = 4 + Keys.size_of_key node.key_type in
  layout_keys node page pair_size 0;
  layout_pointers node page pair_size 0;

  let final_pointer_offset = 12 + (node.capacity * pair_size) in
  (* ensuring serializing the sibling pointer *)
  if node.node_t = Leaf then
    Page.Page.set_int32 page final_pointer_offset
      (Int32.of_int node.pointers.(node.capacity));
  page
