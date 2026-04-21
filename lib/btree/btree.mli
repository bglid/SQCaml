type t
(** Struct for the B+ Tree *)

val serialize : Nodes.t -> int -> Page.Page.t
(** Takes B+ tree node [t] and serializes it to a [Page.t], then writes to disk
    using the [Storage_manager.t]*)
