(* abstraction to make a module for preparing and executing insert statement*)

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VFloat of float
[@@deriving show]

type t = {
  table : Table.t;
  values : value list;
}
[@@deriving show]

(* HACKY for now, new table each time *)
let make (values : value list) : t =
  { table = Table.create_table ~id:777 ~table_name:"temp_table"; values }

let _insert (id : int) (stop_name : string) (rail_name : string) : unit =
  let new_row = Table.make_row ~id ~stop_name ~rail_line:rail_name in
  let temp_table = Table.create_table ~id:777 ~table_name:"temp_table" in
  let _ = Table.add_row ~table:temp_table ~row:new_row in
  Printf.printf "Inserted %d %s %s into Table %d" id stop_name rail_name
    temp_table.table_id
