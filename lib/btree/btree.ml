(* minimal B+ tree struct *)
type t = {
  storage_m : Storage_manager.t;
  key : Keys.t;
  mutable root : Nodes.t;
  mutable root_num : int;
}
