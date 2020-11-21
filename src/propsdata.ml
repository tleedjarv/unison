(* Unison file synchronizer: src/propsdata.ml *)
(* Copyright 2020, Tõivo Leedjärv

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


let debugverbose = Util.debug "propsdata+"


module type S = sig
  val initGetData : unit -> unit
  val keepData : Props.t -> unit
  val getData : unit -> (string * string) list
  val setData : (string * string) list -> unit
end

(* ------------------------------------------------------------------------- *)
(*                                  ACL                                      *)
(* ------------------------------------------------------------------------- *)

module ACL : S = struct

(* Several simple implementations are possible (for example, a Map or an
   association list). There seems to be very little difference in terms
   of performance. Hashtbl has been chosen as it may have a slight scaling
   advantage. In practice, there probably are no tangile differences
   between these simple implementations in most scenarios. *)
let aclStore = Hashtbl.create 25

let exists key = Hashtbl.mem aclStore key

let associate key acl = Hashtbl.add aclStore key acl

let associateOnHost = Remote.registerHostCmd "aclStorAssociate"
  (fun (key, acl) -> Lwt.return
    (if not (exists key) then associate key acl))

let associateRemote host key acl =
  Lwt.ignore_result (associateOnHost host (key, acl))

let associateNew key acl =
  associate key acl;
  let onHost = function
    | (Common.Local, _) -> ()
    | (Common.Remote h, _) -> associateRemote h key acl
  in
  Safelist.iter onHost (Globals.rootsList ())

(* The result value of this function must be deterministic for its input
   (over both roots, and over time, as long as it is the same archive). *)
let hash acl =
  let key = Digest.string acl in
  if not (exists key) then
  associateNew key acl;
  key

let lookupOnHost = Remote.registerHostCmd "aclStorLookup"
  (fun key -> Lwt.return
    (try Some (Hashtbl.find aclStore key) with Not_found -> None))

let lookupRemote host key =
  Lwt_unix.run (lookupOnHost host key)

let lookup key =
  try
    Hashtbl.find aclStore key
  with Not_found ->
    let foundRemote =
      match Globals.roots () with
      | (Common.Local, _), (Local, _) -> None
      | (Common.Local, _), (Remote h, _)
      | (Common.Remote h, _), (Local, _) -> lookupRemote h key
      | (Common.Remote h1, _), (Remote h2, _) ->
        begin
          match lookupRemote h1 key with
          | Some _ as x -> x
          | None -> lookupRemote h2 key
        end
    in
    match foundRemote with
    | Some x ->
      associate key x; (* If found remotely then cache locally *)
      x
    | None -> assert false
(* Not_found indicates a bug (or an incompatible archive) *)

let () = Props.setACLDataFun hash lookup


let newpd = ref []

let initGetData () =
  newpd := []

let keepData props =
  match Props.acl props with
  | None | Some "" -> ()
  | Some s ->
      try
        ignore (Safelist.assoc s !newpd)
      with Not_found ->
        newpd := (s, lookup s) :: !newpd

let getData () =
  !newpd

let setData d =
  ignore (Safelist.map (fun (key, acl) -> associate key acl) d)

end (* module ACL *)


let pdKeyACL = "ACL"

let initGetDataAll () =
  ignore (ACL.initGetData ())

let keepDataAll props =
  ignore (ACL.keepData props)

let getDataAll () =
  let aclList = ACL.getData () in
  [(pdKeyACL, aclList)]

(* This may seem like a wasteful copying around of data, but:
   - the data lists are expected to be very small, typically only a few tens
     to a few hunderd items; but
   - traversing the potentially sizeable archive is needed to prune unused
     items from the lists; and
   - this makes storing the data in the archive independent of the internal
     implementation details; the implementation could be Hashtbl, Map or an
     association list, yet the storage remains unchanged. *)
let rec pruneDataAll = function
  | Update.ArchiveDir (props, children) ->
      keepDataAll props;
      Update.NameMap.iter (fun _ c -> pruneDataAll c) children;
  | ArchiveFile (props, _, _, _) ->
      keepDataAll props
  | ArchiveSymlink (_) -> ()
  | NoArchive -> ()


let preparePropsData archive =
  let t0 = Unix.gettimeofday () in
  debugverbose (fun () -> Util.msg "Pruning shared props data...\n");

  ignore (initGetDataAll ());
  ignore (pruneDataAll archive);

  let t1 = Unix.gettimeofday () in
  debugverbose (fun () ->
    Util.msg "Shared props data pruning took %.3f milliseconds\n"
    ((t1 -. t0) *. 1000.));

  getDataAll ()

let setPropsData pd =
  let t0 = Unix.gettimeofday () in
  debugverbose (fun () -> Util.msg "Restoring shared props data...\n");

  let aclList = try Safelist.assoc pdKeyACL pd with Not_found -> [] in
  ACL.setData aclList;

  let t1 = Unix.gettimeofday () in
  debugverbose (fun () ->
    Util.msg "Shared props data restoring took %.3f milliseconds\n"
    ((t1 -. t0) *. 1000.))

let () = Update.setPropsDataFun preparePropsData setPropsData

