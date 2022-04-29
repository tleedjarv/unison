(* Unison file synchronizer: src/osx.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

val init : bool -> unit
val isMacOSX : bool

val rsrc : bool Prefs.t

type 'a ressInfo =
    NoRess
  | HfsRess of Uutil.Filesize.t
  | AppleDoubleRess of int * float * float * Uutil.Filesize.t * 'a
type ressStamp = unit ressInfo
type info =
  { ressInfo : (Fspath.t * int64) ressInfo;
    finfo : string }

val mressStamp : ressStamp Umarshal.t

(* For backwards compatibility only! Do not use for current code. *)
val minfo_compat : ressStamp Umarshal.t

val defaultInfos :  [> `DIRECTORY | `FILE ] -> info

val getFileInfos : Fspath.t -> Path.local -> [> `DIRECTORY | `FILE ] -> info
val setFileInfos : Fspath.t -> Path.local -> string -> unit

val ressUnchanged :
  'a ressInfo -> 'b ressInfo -> float option -> bool -> bool

val ressFingerprint : Fspath.t -> Path.local -> [> `DIRECTORY | `FILE ] -> Fingerprint.t
val ressLength : 'a ressInfo -> Uutil.Filesize.t

val ressDummy : ressStamp
val ressStampToString : ressStamp -> string

val stamp : info -> ressStamp

(* For backwards compatibility only! Do not use for current code. *)
val stampRev : ressStamp -> info

val openRessIn : Fspath.t -> Path.local -> in_channel
val openRessOut : Fspath.t -> Path.local -> Uutil.Filesize.t -> out_channel
