(* Unison file synchronizer: src/common.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

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


type hostname = string

(* Canonized roots                                                           *)
type host =
    Local
  | Remote of hostname

type root = host * Fspath.t

type 'a oneperpath = ONEPERPATH of 'a list

(* ------------------------------------------------------------------------- *)
(*                       Printing                                            *)
(* ------------------------------------------------------------------------- *)

let root2hostname root =
  match root with
    (Local, _) -> "local"
  | (Remote host, _) -> host

let root2string root =
  match root with
    (Local, fspath) -> Fspath.toPrintString fspath
  | (Remote host, fspath) -> "//"^host^"/"^(Fspath.toPrintString fspath)

(* ------------------------------------------------------------------------- *)
(*                      Root comparison                                      *)
(* ------------------------------------------------------------------------- *)

let compareRoots x y =
  match x,y with
    (Local,fspath1), (Local,fspath2) ->
      (* FIX: This is a path comparison, should it take case
         sensitivity into account ? *)
      Fspath.compare fspath1 fspath2
  | (Local,_), (Remote _,_) -> -1
  | (Remote _,_), (Local,_) -> 1
  | (Remote host1, fspath1), (Remote host2, fspath2) ->
      let result =
        (* FIX: Should this ALWAYS be a case insensitive compare? *)
        compare host1 host2 in
      if result = 0 then
        (* FIX: This is a path comparison, should it take case
           sensitivity into account ? *)
        Fspath.compare fspath1 fspath2
      else
        result

let sortRoots rootList = Safelist.sort compareRoots rootList

(* ---------------------------------------------------------------------- *)

(* IMPORTANT!
   This is the 2.51-compatible version of type [Common.prevState]. It must
   always remain exactly the same as the type [Common.prevState] in version
   2.51.5. This means that if any of the types it is composed of changes then
   for each changed type also a 2.51-compatible version must be created. *)
type prevState251 =
    Previous of Fileinfo.typ * Props.t251 * Os.fullfingerprint * Osx.ressStamp
  | New

type prevState =
    Previous of Fileinfo.typ * Props.t * Os.fullfingerprint
  | New

let ifArchBefore202205 = Umarshal.cond Compat.isArchBefore202205 Osx.ressDummy

let mprevState = Umarshal.(sum2
                             (prod4 Fileinfo.mtyp Props.m Os.mfullfingerprint (ifArchBefore202205 Osx.mressStamp) id id)
                             unit
                             (function
                              | Previous (a, b, c) ->
                                  I21 (a, b, c, Props.Compat.getRessStamp b)
                              | New -> I22 ())
                             (function
                              | I21 (a, b, c, d) ->
                                  Previous (a,
                                    (if Compat.isArchBefore202205 () then
                                       Props.Compat.setRessStamp b d
                                     else b), c)
                              | I22 () -> New))

(* IMPORTANT!
   This is the 2.51-compatible version of type [Common.contentschange]. It
   must always remain exactly the same as the type [Common.contentschange]
   in version 2.51.5. This means that if any of the types it is composed of
   changes then for each changed type also a 2.51-compatible version must be
   created. *)
type contentschange251 =
    ContentsSame
  | ContentsUpdated of Os.fullfingerprint * Fileinfo.stamp251 * Osx.ressStamp

type contentschange =
    ContentsSame
  | ContentsUpdated of Os.fullfingerprint * Fileinfo.stamp

let mcontentschange = Umarshal.(sum2 unit (prod3 Os.mfullfingerprint Fileinfo.mstamp (ifArchBefore202205 Osx.mressStamp) id id)
                                  (function
                                   | (ContentsSame, _) -> I21 ()
                                   | (ContentsUpdated (a, b), c) -> I22 (a, b, c))
                                  (function
                                   | I21 () -> (ContentsSame, Osx.ressDummy)
                                   | I22 (a, b, c) -> (ContentsUpdated (a, b), c)))

type permchange     = PropsSame    | PropsUpdated

let mpermchange = Umarshal.(sum2 unit unit
                              (function
                               | PropsSame -> I21 ()
                               | PropsUpdated -> I22 ())
                              (function
                               | I21 () -> PropsSame
                               | I22 () -> PropsUpdated))

(* IMPORTANT!
   These are the 2.51-compatible versions of types [Common.updateItem] and
   [Common.updateContent]. They must always remain exactly the same as the
   types [Common.updateItem] and [Common.updateContent] in version 2.51.5.
   This means that if any of the types they are composed of changes then
   for each changed type also a 2.51-compatible version must be created. *)
type updateItem251 =
    NoUpdates                         (* Path not changed *)
  | Updates                           (* Path changed in this replica *)
      of updateContent251             (*   - new state *)
       * prevState251                 (*   - summary of old state *)
  | Error                             (* Error while detecting updates *)
      of string                       (*   - description of error *)

and updateContent251 =
    Absent                            (* Path refers to nothing *)
  | File                              (* Path refers to an ordinary file *)
      of Props.t251                   (*   - summary of current state *)
       * contentschange251            (*   - hint to transport agent *)
  | Dir                               (* Path refers to a directory *)
      of Props.t251                   (*   - summary of current state *)
       * (Name.t * updateItem251) list(*   - children;
                                             MUST KEEP SORTED for recon *)
       * permchange                   (*   - did permissions change? *)
       * bool                         (*   - is the directory now empty? *)
  | Symlink                           (* Path refers to a symbolic link *)
      of string                       (*   - link text *)

type updateItem =
    NoUpdates                         (* Path not changed *)
  | Updates                           (* Path changed in this replica *)
      of updateContent                (*   - new state *)
       * prevState                    (*   - summary of old state *)
  | Error                             (* Error while detecting updates *)
      of string                       (*   - description of error *)

and updateContent =
    Absent                            (* Path refers to nothing *)
  | File                              (* Path refers to an ordinary file *)
      of Props.t                      (*   - summary of current state *)
       * contentschange               (*   - hint to transport agent *)
  | Dir                               (* Path refers to a directory *)
      of Props.t                      (*   - summary of current state *)
       * (Name.t * updateItem) list   (*   - children;
                                             MUST KEEP SORTED for recon *)
       * permchange                   (*   - did permissions change? *)
       * bool                         (*   - is the directory now empty? *)
  | Symlink                           (* Path refers to a symbolic link *)
      of string                       (*   - link text *)

let mupdateItem_rec mupdateContent =
  Umarshal.(sum3 unit (prod2 mupdateContent mprevState id id) string
              (function
               | NoUpdates -> I31 ()
               | Updates (a, b) -> I32 (a, b)
               | Error a -> I33 a)
              (function
               | I31 () -> NoUpdates
               | I32 (a, b) -> Updates (a, b)
               | I33 a -> Error a))

let mupdateContent_rec mupdateItem =
  Umarshal.(sum4
              unit
              (prod2 Props.m mcontentschange id id)
              (prod4 Props.m (list (prod2 Name.m mupdateItem id id)) mpermchange bool id id)
              string
              (function
               | Absent -> I41 ()
               | File (a, b) -> I42 (a, (b, Props.Compat.getRessStamp a))
               | Dir (a, b, c, d) -> I43 (a, b, c, d)
               | Symlink a -> I44 a)
              (function
               | I41 () -> Absent
               | I42 (a, (b, c)) ->
                   File ((match b with
                          | ContentsUpdated _ when Compat.isArchBefore202205 () ->
                              Props.Compat.setRessStamp a c
                          | _ -> a), b)
               | I43 (a, b, c, d) -> Dir (a, b, c, d)
               | I44 a -> Symlink a))

let mupdateContent, mupdateItem =
  Umarshal.rec2 mupdateItem_rec mupdateContent_rec

(* Compatibility conversion functions *)

let prev_to_compat251 (prev : prevState) : prevState251 =
  match prev with
  | Previous (typ, props, fp) ->
      Previous (typ, Props.to_compat251 props, fp, Props.Compat.getRessStamp props)
  | New -> New

let prev_of_compat251 (prev : prevState251) : prevState =
  match prev with
  | Previous (typ, props, fp, ress) ->
      Previous (typ, Props.Compat.setRessStamp (Props.of_compat251 props) ress, fp)
  | New -> New

let change_to_compat251 (c : contentschange) ress : contentschange251 =
  match c with
  | ContentsSame -> ContentsSame
  | ContentsUpdated (fp, stamp) ->
      ContentsUpdated (fp, Fileinfo.stamp_to_compat251 stamp, ress)

let change_of_compat251 (c : contentschange251) : (contentschange * _) =
  match c with
  | ContentsSame -> (ContentsSame, None)
  | ContentsUpdated (fp, stamp, ress) ->
      (ContentsUpdated (fp, Fileinfo.stamp_of_compat251 stamp), Some ress)

let rec ui_to_compat251 (ui : updateItem) : updateItem251 =
  match ui with
  | NoUpdates -> NoUpdates
  | Updates (uc, prev) -> Updates (uc_to_compat251 uc, prev_to_compat251 prev)
  | Error s -> Error s

and ui_of_compat251 (ui : updateItem251) : updateItem =
  match ui with
  | NoUpdates -> NoUpdates
  | Updates (uc, prev) -> Updates (uc_of_compat251 uc, prev_of_compat251 prev)
  | Error s -> Error s

and children_to_compat251 l =
  Safelist.map (fun (n, ui) -> (n, ui_to_compat251 ui)) l

and children_of_compat251 l =
  Safelist.map (fun (n, ui) -> (n, ui_of_compat251 ui)) l

and uc_to_compat251 (uc : updateContent) : updateContent251 =
  match uc with
  | Absent -> Absent
  | File (props, change) ->
      File (Props.to_compat251 props,
            change_to_compat251 change (Props.Compat.getRessStamp props))
  | Dir (props, ch, perm, empty) ->
      Dir (Props.to_compat251 props, children_to_compat251 ch, perm, empty)
  | Symlink s -> Symlink s

and uc_of_compat251 (uc : updateContent251) : updateContent =
  match uc with
  | Absent -> Absent
  | File (props, change) ->
      let props' = Props.of_compat251 props
      and (change', ress) = change_of_compat251 change in
      begin match ress with
        | None -> File (props', change')
        | Some ress' -> File (Props.Compat.setRessStamp props' ress', change')
      end
  | Dir (props, ch, perm, empty) ->
      Dir (Props.of_compat251 props, children_of_compat251 ch, perm, empty)
  | Symlink s -> Symlink s

(* ------------------------------------------------------------------------- *)

type status =
  [ `Deleted
  | `Modified
  | `PropsChanged
  | `Created
  | `Unchanged ]

type replicaContent =
  { typ : Fileinfo.typ;
    status : status;
    desc : Props.t;                (* Properties (for the UI) *)
    ui : updateItem;
    size : int * Uutil.Filesize.t; (* Number of items and size *)
    props : Props.t list }         (* Parent properties *)

type direction =
    Conflict of string (* The string is the reason of the conflict *)
  | Merge
  | Replica1ToReplica2
  | Replica2ToReplica1

let direction2string = function
    Conflict _ -> "conflict"
  | Merge -> "merge"
  | Replica1ToReplica2 -> "replica1 to replica2"
  | Replica2ToReplica1 -> "replica2 to replica1"

let isConflict = function
    Conflict _ -> true
  | _ -> false

type difference =
  { rc1 : replicaContent;
    rc2 : replicaContent;
    errors1 : string list;
    errors2 : string list;
    mutable direction : direction;
    default_direction : direction }

type replicas =
    Problem of string       (* There was a problem during update detection *)
  | Different of difference (* Replicas differ *)

type reconItem = {path1 : Path.t; path2 : Path.t; replicas : replicas}

let riAction rc rc' =
  match rc.status, rc'.status with
    `Deleted, _ ->
      `Delete
  | (`Unchanged | `PropsChanged), (`Unchanged | `PropsChanged) ->
      `SetProps
  | _ ->
      `Copy

let rcLength rc rc' =
  if riAction rc rc' = `SetProps then
    Uutil.Filesize.zero
  else
    snd rc.size

let riLength ri =
  match ri.replicas with
    Different {rc1 = {status= `Unchanged | `PropsChanged};
               rc2 = {status= `Unchanged | `PropsChanged}} ->
      Uutil.Filesize.zero (* No contents propagated *)
  | Different {rc1 = rc1; rc2 = rc2; direction = dir} ->
      begin match dir with
        Replica1ToReplica2 -> rcLength rc1 rc2
      | Replica2ToReplica1 -> rcLength rc2 rc1
      | Conflict _         -> Uutil.Filesize.zero
      | Merge              -> Uutil.Filesize.zero (* underestimate :-*)
      end
  | _ ->
      Uutil.Filesize.zero

let fileInfos ui1 ui2 =
  match ui1, ui2 with
    (Updates (File (desc1, ContentsUpdated (fp1, _)),
              Previous (`FILE, desc2, fp2)),
     NoUpdates)
  | (Updates (File (desc1, ContentsUpdated (fp1, _)),
              Previous (`FILE, desc2, fp2)),
     Updates (File (_, ContentsSame), _))
  | (NoUpdates,
     Updates (File (desc2, ContentsUpdated (fp2, _)),
              Previous (`FILE, desc1, fp1)))
  | (Updates (File (_, ContentsSame), _),
     Updates (File (desc2, ContentsUpdated (fp2, _)),
              Previous (`FILE, desc1, fp1)))
  | (Updates (File (desc1, ContentsUpdated (fp1, _)), _),
     Updates (File (desc2, ContentsUpdated (fp2, _)), _)) ->
       (desc1, fp1, desc2, fp2)
  | _ ->
      raise (Util.Transient "Can't diff")

let problematic ri =
  match ri.replicas with
    Problem _      -> true
  | Different diff -> isConflict diff.direction

let partiallyProblematic ri =
  match ri.replicas with
    Problem _      ->
      true
  | Different diff ->
     isConflict diff.direction || diff.errors1 <> [] || diff.errors2 <> []

let isDeletion ri =
  match ri.replicas with
    Different {rc1 = rc1; rc2 = rc2; direction = rDir} ->
      (match rDir, rc1.typ, rc2.typ with
        Replica1ToReplica2, `ABSENT, _ -> true
      | Replica2ToReplica1, _, `ABSENT -> true
      | _ -> false)
  | _ -> false

let rcType rc = Fileinfo.type2string rc.typ

let riFileType ri =
  match ri.replicas with
    Different {rc1 = rc1; rc2 = rc2; default_direction = dir} ->
      begin match dir with
        Replica2ToReplica1 -> rcType rc2
      | _		   -> rcType rc1
      end
  | _ -> "nonexistent"
