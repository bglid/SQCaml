type t =
  | TVarchar of int
  | TInteger
[@@deriving show]

type value =
  | Varchar of string
  | Integer of Int32.t
[@@deriving show]

let less_than (k1 : value) (k2 : value) : bool =
  match (k1, k2) with
  | Integer n1, Integer n2 -> n1 < n2
  | Varchar v1, Varchar v2 -> v1 < v2
  | _ -> failwith "Incompatible keys"

let equals (k1 : value) (k2 : value) : bool =
  match (k1, k2) with
  | Integer n1, Integer n2 -> n1 = n2
  | Varchar v1, Varchar v2 -> v1 = v2
  | _ -> failwith "Incompatible keys"

let greater_than (k1 : value) (k2 : value) : bool =
  match (k1, k2) with
  | Integer n1, Integer n2 -> n1 > n2
  | Varchar v1, Varchar v2 -> v1 > v2
  | _ -> failwith "Incompatible keys"

let leq_than (k1 : value) (k2 : value) : bool = less_than k1 k2 || equals k1 k2

let greq_than (k1 : value) (k2 : value) : bool =
  greater_than k1 k2 || equals k1 k2

let string_of_key (k : value) : string =
  match k with
  | Integer n -> Printf.sprintf "Integer: %d" (Int32.to_int n)
  | Varchar v -> Printf.sprintf "Varchar: %s" v

let size_of_key (key_type : t) : int =
  match key_type with
  | TVarchar s -> s
  | TInteger -> 4

let empty_key (key_type : t) : value =
  match key_type with
  | TVarchar n -> Varchar (String.make n '\"')
  | TInteger -> Integer Int32.max_int
