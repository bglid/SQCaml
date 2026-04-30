(* ops for predicates*)
type pred_op =
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
[@@deriving show]

type predicate = {
  field : string;
  op : pred_op;
  value : Constant.t;
}
[@@deriving show]

type t = {
  fields : string list;
  predicate : predicate option;
}
[@@deriving show]

(* kinda stupid for now but will be better for future*)
let make ?predicate (fields : string list) : t = { fields; predicate }
let default_fields = [ "id"; "stop_name"; "rail_line" ]

let match_fields (res : string) (row : Table.row_t) =
  match res with
  | "id" -> Int.to_string row.id
  | "stop_name" -> row.stop_name
  | "rail_line" -> row.rail_line
  | _ -> failwith "Out of schema!!!"

let row_field_value (field : string) (row : Table.row_t) : Constant.t =
  match String.lowercase_ascii field with
  | "id" -> Constant.make_int (Int32.of_int row.id)
  | "stop_name" -> Constant.ConstStr row.stop_name
  | "rail_line" -> Constant.ConstStr row.rail_line
  | _ -> failwith "Out of schema!!!"

let render_fields (fields : string list) row =
  fields
  |> List.map (fun field -> match_fields (String.lowercase_ascii field) row)
  |> String.concat ", "

let comp_constants (op : pred_op) (left : Constant.t) (right : Constant.t) :
    bool =
  match (left, right) with
  | Constant.ConstInt l, Constant.ConstInt r -> begin
      match op with
      | Eq -> l = r
      | Neq -> l <> r
      | Lt -> l < r
      | Gt -> l > r
      | Leq -> l <= r
      | Geq -> l >= r
    end
  | Constant.ConstStr l, Constant.ConstStr r -> begin
      match op with
      | Eq -> l = r
      | Neq -> l <> r
      | Lt -> l < r
      | Gt -> l > r
      | Leq -> l <= r
      | Geq -> l >= r
    end
  | _ -> failwith "Bad!"

let row_good_predicate (pred : predicate) (row : Table.row_t) : bool = 
  let left = row_field_value pred.field row in
  comp_constants pred.op left pred.value

let execute_select ?prepped_select (db : Db_session.t) : string =
  let cursor = Cursor.tree_start db.index in
  let rec cursor_select (c : Cursor.t) (acc : string list) : string list =
    if c.end_of_table then
      List.rev acc
    else
      let row_page_num = Cursor.cursor_value c in

      let page =
        Storage_manager.get_block ~storage_m:db.storage_manager
          ~block_num:row_page_num
      in

      let row = Table.deserialize_from_page page in
      Cursor.cursor_advance c;
      match prepped_select with
      | None -> begin
          let str_row =
            Int.to_string row.id ^ ", " ^ row.stop_name ^ ", " ^ row.rail_line
            ^ "\n"
          in
          cursor_select c (str_row :: acc)
        end
        (* gross hack*)
      | Some prep -> 
          let pred_pass = 
            match prep.predicate with
              | None -> true
              | Some p -> row_good_predicate p row
              in
        (* basically will only "query" fields if they match*)
          if pred_pass then
                let str_row = render_fields prep.fields row ^ "\n" in
                cursor_select c (str_row :: acc)
            else 
                cursor_select c acc
        in

  let res = cursor_select cursor [] in
  List.fold_left (fun acc l -> acc ^ l) "" res
