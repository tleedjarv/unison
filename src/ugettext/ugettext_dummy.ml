(* Unison file synchronizer: src/ugettext/ugettext_dummy.ml *)

(* No-op implementation for when gettext is not available
   or chosen not to be linked. *)

external s_ : string -> string = "%identity"
external f_ : _ format6 -> _ format6 = "%identity"

let sn_ s p n = if n = 1 then s else p

let fn_ s p n = if n = 1 then s else p

let gettext_argspecs = []
