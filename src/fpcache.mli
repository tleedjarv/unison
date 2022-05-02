(* Unison file synchronizer: src/fpcache.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

(* Initialize the cache *)
val init : bool -> bool -> string -> unit

(* Close the cache file and clear the in-memory cache *)
val finish : unit -> unit

(* Get the fingerprint of a file, possibly from the cache *)
val fingerprint :
  ?newfile:bool ->
  bool -> Fspath.t -> Path.local -> Fileinfo.t -> Os.fullfingerprint option ->
  Props.t * Os.fullfingerprint * Fileinfo.stamp

(* Add an entry to the cache *)
val save :
  Path.local ->
  Props.t * Os.fullfingerprint * Fileinfo.stamp -> unit

(****)

val dataAndExtClearlyUnchanged :
  bool -> Path.local -> (_ Props.props, _) Fileinfo.info -> Props.t -> Fileinfo.stamp -> bool * bool
val dataAndExtClearlyUnchanged' :
  bool -> Path.local -> Props.t -> Props.t -> bool
