(* minimal B+ tree struct *)
type t = {
  storage_m : Storage_manager.t; (* file used to store to disk *)
  key : Keys.t;
  mutable root : Nodes.t;
  mutable root_num : int;
}

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

let get_num_keys (block_size : int) (key_type : Keys.t) : int =
  (* 12 bytes needed for metadata + 4 bytes for final sib pointer*)
  (block_size - 16) / (4 + Keys.size_of_key key_type)

let unused_pointer_serial_i32 = Int32.of_string "0xDDDDDDDD"
let unused_pointer_serial = Int32.to_int unused_pointer_serial_i32
(*3722304989*)
(*0xDDDDDDDD *)

let deserialize (page : Page.Page.t) (key_type : Keys.t) (block_size : int) :
    Nodes.t =
  (* read node type *)
  let node_type = Nodes.int32_to_node_t (Page.Page.get_int32 page 0) in
  (* Read parent*)
  let parent = Int32.to_int (Page.Page.get_int32 page 4) in
  (* Read N the cur num of keys *)
  let cur_size = Int32.to_int (Page.Page.get_int32 page 8) in
  (* Get the capacity using block size and k type *)
  let capacity = get_num_keys block_size key_type in
  let keys = Array.init capacity (fun _ -> Keys.empty_key key_type) in
  let pointers = Array.init (capacity + 1) (fun _ -> unused_pointer_serial) in
  let pair_size = 4 + Keys.size_of_key key_type in

  (* reading keys and pointers - not doing this one recursively*)
  for i = 0 to cur_size - 1 do
    let pointer_offset = 12 + (i * pair_size) in
    let key_offset = 12 + (i * pair_size) + 4 in
    (* reading in the pointers from bytes *)
    pointers.(i) <- Int32.to_int (Page.Page.get_int32 page pointer_offset);
    (* reading the keys from bytes *)
    match key_type with
    | Keys.TVarchar n ->
        let str = Page.Page.get_string_raw page key_offset n in
        keys.(i) <- Keys.Varchar str
    | Keys.TInteger ->
        let num = Page.Page.get_int32 page key_offset in
        keys.(i) <- Keys.Integer num
  done;

  (* again final pointer read *)
  let last_pointer_offset = 12 + (cur_size * pair_size) in
  pointers.(cur_size) <-
    Int32.to_int (Page.Page.get_int32 page last_pointer_offset);
  (* sibling pointer is last pointer in array at index capacity *)
  let sib_pointer_offset = 12 + (capacity * pair_size) in
  if node_type = Leaf then
    pointers.(capacity) <-
      Int32.to_int (Page.Page.get_int32 page sib_pointer_offset);

  { node_t = node_type; parent; cur_size; keys; pointers; capacity; key_type }

(* writing nodes to disk *)
let write_node (btree : t) (node : Nodes.t) (n : int) : unit =
  let block_size = File_manager.get_blocksize btree.storage_m.file_manager in
  let page = serialize node block_size in
  Storage_manager.update_block_num ~storage_m:btree.storage_m ~block_num:n ~page

let write_node_append (btree : t) (node : Nodes.t) : int =
  let block_size = File_manager.get_blocksize btree.storage_m.file_manager in
  let page = serialize node block_size in
  let block = Storage_manager.append ~storage_m:btree.storage_m ~page in
  Page.Block.block_num block

(* getting a block from the btree and deserialize it into a node *)
let get_node (btree : t) (p : int) : Nodes.t =
  let block_size = File_manager.get_blocksize btree.storage_m.file_manager in
  let page =
    Storage_manager.get_block ~storage_m:btree.storage_m ~block_num:p
  in
  deserialize page btree.key block_size

(* getting the key value at the max of the node*)
let get_node_max_key (node : Nodes.t) : Keys.value =
  if node.cur_size = 0 then
    failwith "Empty node!!!!!! Please pass in an initialized node"
  else
    node.keys.(node.cur_size - 1)

let empty_node (btree : t) : Nodes.t =
  let block_size = File_manager.get_blocksize btree.storage_m.file_manager in
  let key_type = btree.key in
  let capacity = get_num_keys block_size key_type in
  {
    Nodes.node_t = Leaf;
    Nodes.parent = 0;
    Nodes.cur_size = 0;
    Nodes.keys = Array.init capacity (fun _ -> Keys.empty_key key_type);
    Nodes.pointers = Array.init (capacity + 1) (fun _ -> unused_pointer_serial);
    Nodes.capacity;
    Nodes.key_type;
  }

(* Make an empty btree initialized to disk *)
let create (storage_m : Storage_manager.t) (key_type : Keys.t) : t =
  let block_size = File_manager.get_blocksize storage_m.file_manager in
  let meta = Storage_manager.get_head_page ~storage_manager:storage_m in
  Page.Page.set_int32 meta 4 (Int32.of_int 1);
  Storage_manager.update_block_num ~storage_m ~block_num:0 ~page:meta;
  let capacity = get_num_keys block_size key_type in
  let root_node =
    {
      Nodes.node_t = Leaf;
      Nodes.parent = 0;
      Nodes.cur_size = 0;
      Nodes.keys = Array.init capacity (fun _ -> Keys.empty_key key_type);
      Nodes.pointers =
        Array.init (capacity + 1) (fun _ -> unused_pointer_serial);
      Nodes.capacity;
      Nodes.key_type;
    }
  in
  let root_page = serialize root_node block_size in
  let _ = Storage_manager.append ~storage_m ~page:root_page in
  { storage_m; key = key_type; root = root_node; root_num = 1 }

let open_btree (storage_m : Storage_manager.t) (key_type : Keys.t) : t =
  let block_size = File_manager.get_blocksize storage_m.file_manager in
  let meta = Storage_manager.get_head_page ~storage_manager:storage_m in
  let root_num = Int32.to_int (Page.Page.get_int32 meta 4) in
  if root_num = 0 then
    (* create btree instead *)
    create storage_m key_type
  else
    let root_page = Storage_manager.get_block ~storage_m ~block_num:root_num in
    let root_node = deserialize root_page key_type block_size in
    { storage_m; key = key_type; root = root_node; root_num }

let print_tree (tree : t) : string =
  let rec print_node (page_num : int) (indent : int) : string =
    let node = get_node tree page_num in
    let prefix = String.make indent ' ' in

    match node.node_t with
    | Nodes.Leaf -> prefix ^ Nodes.print_leaf_node node
    | Nodes.Internal ->
        let header = prefix ^ Nodes.print_internal_node node in
        let keys =
          String.concat ""
            (List.init node.cur_size (fun i ->
                 match node.keys.(i) with
                 | Keys.Integer n ->
                     prefix ^ Printf.sprintf "- %d\n" (Int32.to_int n)
                 | Keys.Varchar v -> prefix ^ Printf.sprintf "- %s\n" v))
        in
        let children =
          String.concat ""
            (List.init (node.cur_size + 1) (fun i ->
                 let child_page_num = node.pointers.(i) in
                 prefix
                 ^ Printf.sprintf " child %d -> page %d\n" i child_page_num
                 ^ print_node child_page_num (indent + 4)))
        in
        header ^ keys ^ children
  in
  print_node tree.root_num 0

let create_new_root (tree : t) ~(left_child_page_num : int)
    ~(left_child : Nodes.t) ~(right_child_page_num : int) : unit =
  (*create new root*)
  let root = empty_node tree in
  root.node_t <- Nodes.Internal;
  root.parent <- 0;
  root.cur_size <- 1;
  (* update nodes as children*)
  root.pointers.(0) <- left_child_page_num;
  root.keys.(0) <- get_node_max_key left_child;
  root.pointers.(1) <- right_child_page_num;
  write_node tree root tree.root_num
