(* Unison file synchronizer: src/hash.mli *)
(* Copyright 2022, Tõivo Leedjärv (see COPYING for details) *)

type t = string

val string : string -> t

val channel : in_channel -> int -> t

module MD5 : sig
  val string : string -> t

  val toString : t -> string
end
