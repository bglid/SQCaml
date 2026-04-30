type t =
  | TVarchar of int
  | TInteger
[@@deriving show]

type value =
  | Varchar of string
  | Integer of Int32.t
[@@deriving show]

val less_than : value -> value -> bool
(** Takes in two key values and returns the bool of v1 < v2 *)

val equals : value -> value -> bool
(** Takes in two key values and returns the bool of v1 = v2 *)

val leq_than : value -> value -> bool
(** Takes in two key values and returns the bool of v1 <= v2 *)

val greater_than : value -> value -> bool
(** Takes in two key values and returns the bool of v1 > v2 *)

val greq_than : value -> value -> bool
(** Takes in two key values and returns the bool of v1 >= v2 *)

val string_of_key : value -> string
(** Takes in key value [k] and returns a string of that key value *)

val size_of_key : t -> int
(** Returns in bytes the size of [key_type]*)

val empty_key : t -> value
(** Takes in [key_type] and creates one with an empty value *)
