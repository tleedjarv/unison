(* Unison file synchronizer: src/ugettext/ugettext_intf.ml *)

(* The code in this signature is in large part copied from the signature
   of [Gettext.Program] of ocaml-gettext by Sylvain Le Gall (LGPL v2.1 or
   later, with OCaml static compilation exception). *)

(* The signature of [Gettext.Program] must not be used directly to avoid
   dependency on ocaml-gettext. Additionally, this gives us full control
   over the interface exposed for Unison. *)
module type S = sig

  val s_ : string -> string

  val f_ :
    ('a, 'b, 'c, 'c, 'c, 'f) Stdlib.format6 ->
    ('a, 'b, 'c, 'c, 'c, 'f) Stdlib.format6

  val sn_ : string -> string -> int -> string

  val fn_ :
    ('a, 'b, 'c, 'c, 'c, 'f) Stdlib.format6 ->
    ('a, 'b, 'c, 'c, 'c, 'f) Stdlib.format6 ->
    int ->
    ('a, 'b, 'c, 'c, 'c, 'f) Stdlib.format6
  (* To explain why the types above look the way they do: Unfortunately,
     ocaml-gettext has slightly incorrect types in the Gettext.Program
     signature [as of 2026-04]. ('a, 'b, 'c, 'c, 'c, 'f) Stdlib.format6
     should actually be ('a, 'b, 'c, 'd, 'e, 'f) Stdlib.format6. Fortunately,
     this does not break any format strings in Unison, and it shouldn't
     cause much trouble when ocaml-gettext upstream changes the signature;
     however Unison is stuck with this signature for as long as the current
     ocaml-gettext version needs to be supported. *)

end
