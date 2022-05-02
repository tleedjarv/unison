(* Unison file synchronizer: src/props.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

(* File properties: time, permission, length, etc. *)

type t251
type _ props
type basic = [`Basic] props
type t = [`Full] props
val m : t Umarshal.t
val mbasic : basic Umarshal.t
val to_compat251 : t -> t251
val of_compat251 : t251 -> t
val basic_to_compat251 : basic -> t251
val basic_of_compat251 : t251 -> basic
val dummy : t
val basicDummy : basic
val hash : t -> int -> int
val hash251 : t251 -> int -> int
val similar : t -> t -> bool
val override : _ props -> t -> t
val strip : t -> t
val diff : t -> t -> t
val toString : t -> string
val syncedPartsToString : t -> string
val set : Fspath.t -> Path.local -> [`Set | `Update] -> t -> unit
val get' : Unix.LargeFile.stats -> basic
val get : Fspath.t -> Path.local -> Unix.LargeFile.stats -> [> `FILE | `DIRECTORY] -> t
val getWithRess : Fspath.t -> Path.local -> Unix.LargeFile.stats -> [> `FILE | `DIRECTORY] -> basic
val check : Fspath.t -> Path.local -> Unix.LargeFile.stats -> t -> unit
val init : bool -> unit

val same_time : _ props -> t -> bool
val length : _ props -> Uutil.Filesize.t
val totalCombinedLength : t -> Uutil.Filesize.t
val setLength : t -> Uutil.Filesize.t -> t
val time : _ props -> float
val setTime : t -> float -> t
val perms : _ props -> int

val extUnchanged : _ props -> _ props -> ?t0:float -> bool -> bool
val ressLength : _ props -> Uutil.Filesize.t

type lengths
val mlengths : lengths Umarshal.t
val lengthsDummy : lengths
val lengths : _ props -> lengths
val length' : lengths -> Uutil.Filesize.t
val ressLength' : lengths -> Uutil.Filesize.t

val fileDefault : basic
val fileSafe : t
val dirDefault : basic

val syncModtimes : bool Prefs.t
val permMask : int Prefs.t
val dontChmod : bool Prefs.t

(* We are reusing the directory length to store a flag indicating that
   the directory is unchanged *)
type dirChangedStamp
val mdirChangedStamp : dirChangedStamp Umarshal.t
val freshDirStamp : unit -> dirChangedStamp
val changedDirStamp : dirChangedStamp
val setDirChangeFlag : t -> dirChangedStamp -> int -> t * bool
val dirMarkedUnchanged : t -> dirChangedStamp -> int -> bool

val validatePrefs: unit -> unit

type fingerprint
val mfingerprint : fingerprint Umarshal.t
val fingerprintDummy : fingerprint
val fingerprintToString : fingerprint -> string
val reasonForFingerprintMismatch : fingerprint -> fingerprint -> string
val fingerprintHash : fingerprint -> int
val fingerprintEqual : fingerprint -> fingerprint -> bool
val extFingerprint : Fspath.t -> Path.local ->  [> `FILE | `DIRECTORY] -> fingerprint

(* Functions for backwards compatibility with older versions.
   DO NOT USE for any other purpose! *)
module Compat : sig
  val getRessStamp : _ props -> Osx.ressStamp
  val setRessStamp : t -> Osx.ressStamp -> t
  val basic_setRessStamp : basic -> Osx.ressStamp -> basic

  val length : lengths -> Uutil.Filesize.t
  val ressLength : lengths -> Uutil.Filesize.t
  val lengths_of_compat251 : Uutil.Filesize.t * Uutil.Filesize.t -> lengths

  val getRessFingerprint : fingerprint -> Fingerprint.t
  val setRessFingerprint : Fingerprint.t -> fingerprint
end
