(** Holds variants for different types of nodes *)
type node_type =
  | Leaf
  | Internal
[@@deriving show]

type t [@@deriving show]
(** Data structure for nodes *)
