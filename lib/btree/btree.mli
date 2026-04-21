type t
(** Struct for the B+ Tree *)

val serialize : Nodes.t -> int -> Page.Page.t
(** Takes B+ tree node [t] and serializes it to a [Page.t], then writes to disk
    using the [Storage_manager.t]*)

val get_num_keys : int -> Keys.t -> int
(** Get number of keys from block *)

val deserialize : Page.Page.t -> Keys.t -> int -> Nodes.t
(** Convert page into B+ tree node struct *)
