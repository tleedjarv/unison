(* Unison file synchronizer: src/xxhash.mli *)
(* Copyright 1999-2023, Benjamin C. Pierce (see COPYING for details) *)

module type XXH = sig
  type t = string
  val channel : in_channel -> int -> t
end

module XXH3_64 : XXH
