(* abstraction to make a module for preparing and executing insert statement*)

type t = {
  table : Table.t;
  fields : string list;
  values : Constant.t list;
}
[@@deriving show]

(* HACKY for now, new table each time *)
let make (tablename : string) (fields : string list) (values : Constant.t list)
    : t =
  { table = Table.create_table ~id:777 ~table_name:tablename; fields; values }

let execute_insert (preped_insert : t) : string =
  (* let new_table = Table.add_row ~table:temp_table ~row:new_row in *)
  let new_fields =
    List.fold_left
      (fun acc l ->
        match l with
        | Constant.ConstStr s -> acc ^ " " ^ s
        | Constant.ConstInt d -> acc ^ " " ^ Int.to_string (Int32.to_int d))
      "" preped_insert.values
  in
  (* new_fields *)
  "Inserted" ^ new_fields ^ "into" ^ preped_insert.table.table_name

(* Printf.printf "Inserted %d %s %s into Table %d" id stop_name rail_name *)
(*   preped_insert.table.table_name *)
