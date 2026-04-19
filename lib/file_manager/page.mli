(** Blocks are representative of the Blocks on the Disk *)
module Block : sig
  type t

  val file_name : t -> string
  (** Return the file name upon giving the block*)

  val block_num : t -> int
  (** Get the block offset, as an int, in the file given the block id *)

  val make : filename:string -> block_num:int -> t
  (** Init block string and int here: Take a labeled arg ~filename, which is a
      string, and an arg ~block_num, which is an int, to create a Block type t.
      Using labels for clarity in API design here*)

  val to_string : t -> string
  (** Converts Block Id to string representation *)

  val eq : t -> t -> bool
  (** Boolean check on if blocks are the same *)
end

(** Page is a module that allows us to buffer bytes in memory. It allows us to
    do work on the blocks before writing them back to disk*)
module Page : sig
  type t

  val make : block_size:int -> t
  (** Calls Bytes.make to init a block of size ~block_size and returns an init
      page. Uses null char '\000' *)

  val from_bytes : bytes -> t
  (** Return page from bytes *)

  val get_int32 : t -> int -> int32
  (** Given page t, and offset in int, return the 32-bit integer that page t
      belongs to, via Bytes.get_int32_ne *)

  val set_int32 : t -> int -> int32 -> unit
  (** Given page t, and offset in int, and a int32, set page in the 32-bit
      integer, from the offset to the passed int32, via Bytes.set_int32_ne *)

  val contents : t -> bytes
  (** Return the bytes given page t *)

  val get_bytes : t -> int -> bytes
  (** Using page t and offset int, get the bytes back *)

  val set_bytes : t -> int -> bytes -> unit
  (** Using page t, offset int, and bytes b, get the bytes back *)

  val get_string : t -> int -> string
  (** With page and offset, return the bytes in string. Uses Bytes.to_string *)

  val set_string : t -> int -> string -> unit
  (** With page offset and the string, Set bytes using Bytes.of_string *)

  val set_string_raw : t -> int -> string -> unit
  (** Gvne page, offset and the string, get bytes of the string and set using
      Bytes.blit*)

  val get_string_raw : t -> int -> int -> string
  (** Get the raw string of length n using Bytes.sub *)

  val max_len : int -> int
  (**  *)

  val ( = ) : t -> t -> bool
  (** Wrapper for Bytes.equal *)

  val zero_out : t -> unit
  (** Fill page t with 0 using Bytes.fill *)
end
