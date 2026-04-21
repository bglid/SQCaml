module Block = struct
  type t = string * int [@@deriving show]

  let file_name (filename, _) = filename
  let block_num (_, blocknum) = blocknum
  let make ~filename ~block_num = (filename, block_num)
  let to_string (filename, blocknum) = Printf.sprintf "%s, %d" filename blocknum
  let eq (fn1, bn1) (fn2, bn2) = fn1 = fn2 && bn1 = bn2
end

module Page = struct
  type t = bytes [@@deriving show]

  let make ~block_size = Bytes.make block_size '\000'
  let from_bytes b = b
  let get_int32 page offset = Bytes.get_int32_ne page offset
  let set_int32 page offset b32 = Bytes.set_int32_ne page offset b32
  let contents page = page

  let get_bytes page offset =
    let len = Int32.to_int (get_int32 page offset) in
    Bytes.sub page (offset + 4) len

  let set_bytes page offset b =
    assert (offset + 4 + Bytes.length b <= Bytes.length page);
    let len = Bytes.length b in
    let _ = set_int32 page offset (Int32.of_int len) in
    Bytes.blit b 0 page (offset + 4) len

  let get_string page offset = Bytes.to_string (get_bytes page offset)
  let set_string page offset s = set_bytes page offset (Bytes.of_string s)

  let set_string_raw page offset s =
    let b = Bytes.of_string s in
    let len = Bytes.length b in
    Bytes.blit b 0 page offset len

  let get_string_raw page offset n = Bytes.to_string (Bytes.sub page offset n)
  let max_len offset = offset + 4
  let ( = ) = Bytes.equal
  let zero_out page = Bytes.fill page 0 (Bytes.length page) '\000'
end
