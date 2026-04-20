type t =
  | TVarchar of int
  | TInteger
[@@deriving show]

type value [@@deriving show]

val less_than : value -> value -> bool
val equals : value -> value -> bool
val leq_than : value -> value -> bool
val greater_than : value -> value -> bool
val greq_than : value -> value -> bool
val string_of_key : value -> string
val size_of_key : t -> int
val empty_key : t -> value
