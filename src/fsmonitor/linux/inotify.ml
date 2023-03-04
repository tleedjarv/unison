(*
 * Copyright (C) 2006-2008 Vincent Hanquez <vincent@snarc.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * Inotify OCaml binding
 *)

type select_event =
        | S_Access
        | S_Attrib
        | S_Close_write
        | S_Close_nowrite
        | S_Create
        | S_Delete
        | S_Delete_self
        | S_Modify
        | S_Move_self
        | S_Moved_from
        | S_Moved_to
        | S_Open
        | S_Dont_follow
        | S_Mask_add
        | S_Oneshot
        | S_Onlydir
        | S_Excl_unlink
        (* convenience *)
        | S_Move
        | S_Close
        | S_All

type type_event =
        | Access
        | Attrib
        | Close_write
        | Close_nowrite
        | Create
        | Delete
        | Delete_self
        | Modify
        | Move_self
        | Moved_from
        | Moved_to
        | Open
        | Ignored
        | Isdir
        | Q_overflow
        | Unmount

let string_of_event = function
        | Access -> "ACCESS"
        | Attrib -> "ATTRIB"
        | Close_write -> "CLOSE_WRITE"
        | Close_nowrite -> "CLOSE_NOWRITE"
        | Create -> "CREATE"
        | Delete -> "DELETE"
        | Delete_self -> "DELETE_SELF"
        | Modify -> "MODIFY"
        | Move_self -> "MOVE_SELF"
        | Moved_from -> "MOVED_FROM"
        | Moved_to -> "MOVED_TO"
        | Open -> "OPEN"
        | Ignored -> "IGNORED"
        | Isdir -> "ISDIR"
        | Q_overflow -> "Q_OVERFLOW"
        | Unmount -> "UNMOUNT"

let int_of_wd wd = wd

type wd = int
type event = wd * type_event list * int32 * string option

external init : unit -> Unix.file_descr = "stub_inotify_init"
external add_watch : Unix.file_descr -> string -> select_event list -> wd
                   = "stub_inotify_add_watch"
external rm_watch : Unix.file_descr -> wd -> unit = "stub_inotify_rm_watch"
external convert : bytes -> int -> (wd * type_event list * int32 * int)
                 = "stub_inotify_convert"
external struct_size : unit -> int = "stub_inotify_struct_size"
external min_buf_size : unit -> int = "stub_inotify_min_buf_size"

let struct_size = struct_size ()
let min_buf_size = min_buf_size ()

let c_string buf ofs len =
  Bytes.sub_string buf ofs
    (try Bytes.index_from buf ofs '\000' - ofs with Not_found -> len)

let parse buf bofs blen =
  let rec parse_aux ofs acc =
    if ofs >= blen then
      List.rev acc
    else
      let struct_end = ofs + struct_size in
      let wd, l, cookie, len = convert buf ofs in
      let s = if len > 0 then Some (c_string buf struct_end len) else None in
      parse_aux (struct_end + len) ((wd, l, cookie, s) :: acc)
  in
  parse_aux bofs []
