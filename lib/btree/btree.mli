type t
(** Struct for the B+ Tree *)

val serialize : Nodes.t -> int -> Page.Page.t
(** Takes B+ tree node [t] and serializes it to a [Page.t], then writes to disk
    using the [Storage_manager.t]*)

val get_num_keys : int -> Keys.t -> int
(** Get number of keys from block *)

val deserialize : Page.Page.t -> Keys.t -> int -> Nodes.t
(** Convert page into B+ tree node struct *)

val write_node : t -> Nodes.t -> int -> unit
(** Take B+ tree [t] [node] and offset n into the file. Updates block within
    file via storage manager *)

val write_node_append : t -> Nodes.t -> int
(** Append a block in B+ tree and return offset to new blocks location. Used
    when creating a new node *)

val get_node : t -> int -> Nodes.t
(** Get a block from the btree [t] and deserializeit using the pointer [p] into
    a btree node *)

val open_btree : Storage_manager.t -> Keys.t -> t
(** Opens btree given [storage manager] and [key type]. If it doesn't exist,
    creates a new btree *)
