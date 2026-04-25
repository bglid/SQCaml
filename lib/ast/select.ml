(* abstraction to make a module for preparing and executing select statement*)

type t = {
  fields : string list;
  table : Table.t;
}
[@@deriving show]

(* HACKY for now, new table each time *)
let make (fields : string list) (tablename : string) : t =
  { table = Table.create_table ~id:777 ~table_name:tablename; fields }

let execute_select (_ : Db_session.t) (preped_select : t) : string =
  let selected_fields =
    List.fold_left
      (fun acc l ->
        match l with
        (* | "id" -> acc ^ " 1" *)
        | "stop_name" -> acc ^ "englewood"
        | "rail_line" -> acc ^ " G"
        | _ -> "not a valid field in the table")
      "" preped_select.fields
  in
  selected_fields
