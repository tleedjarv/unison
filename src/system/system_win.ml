(* Unison file synchronizer: src/system/system_win.ml *)
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

(*XXXX

- Use SetConsoleOutputCP/SetConsoleCP in text mode ???
http://www.codeproject.com/KB/cpp/unicode_console_output.aspx?display=Print

*)

include System_generic


let fixPath f = String.map (function '/' -> '\\' | c -> c) f
let winRootRx = Rx.rx "[a-zA-Z]:[/\\].*"
let winUncRx = Rx.rx "[/\\][/\\][^/\\]+[/\\][^/\\]+[/\\].*"
let extendedPath f =
  if Rx.match_string winRootRx f then
    fixPath ("\\\\?\\" ^ f)
  else if Rx.match_string winUncRx f then
    fixPath ("\\\\?\\UNC" ^ String.sub f 1 (String.length f - 1))
  else
    f

(****)

(* Intended to be used only with Fspath as input. *)
let fspathExtFromString f = extendedPath f

(****)

let path8 = Unicode.from_utf_16(*_filename*)

let sys_error e =
  match e with
    Unix.Unix_error (err, _, "") ->
      raise (Sys_error (Unix.error_message err))
  | Unix.Unix_error (err, _, s) ->
      raise (Sys_error (Format.sprintf "%s: %s" s (Unix.error_message err)))
  | _ ->
      raise e

(****)

let rename f1 f2 =
  (* Comment from original implementation:
     Windows Unicode API: when a file cannot be renamed due to a sharing
     violation error or an access denied error, retry for up to 1 second,
     in case the file is temporarily opened by an indexer or an anti-virus. *)
  let delay = ref 0.01 in
  let rec ren_aux () =
    try
      Unix.rename f1 f2
    with
     (Unix.Unix_error (Unix.EACCES, _, _)
    | Unix.Unix_error (Unix.EUNKNOWNERR (-32), _, _)) as e ->
                       (* ERROR_SHARING_VIOLATION *)
        if (!delay < 1.) then begin
          Unix.sleepf !delay;
          delay := !delay *. 2.;
          ren_aux ()
        end else
          raise e
    | e -> raise e
  in
  ren_aux ()

let chown _ _ _ = raise (Unix.Unix_error (Unix.ENOSYS, "chown", ""))

let openfile f flags perm =
  let fd = Unix.openfile f flags perm in
  (* Comment from original implementation:
     Windows: implement somewhat the O_APPEND flag, so that appending
     lines to a profile (ignored files, for instance) works instead of
     overwriting the beginning of the file (the file pointer is moved to
     the end when the file is opened, rather that each time something is
     written, which is good enough here) *)
  if List.mem Unix.O_APPEND flags then
    ignore (Unix.LargeFile.lseek fd 0L Unix.SEEK_END);
  fd

let chdir = Sys.chdir
external long_name : string -> string = "win_long_path_name"
let getcwd () =
  try
    let s = long_name (Sys.getcwd ()) in
    (* Convert the drive letter to uppercase *)
    match s.[0] with
    | 'a' .. 'z' -> String.capitalize_ascii s
    | _ -> s
  with e -> sys_error e

let opendir f =
  let h = Unix.opendir f in
  { readdir =  (fun () -> Unix.readdir h);
    closedir = (fun () -> Unix.closedir h) }

let open_in_gen = open_in_gen
let open_out_gen = open_out_gen

(****)

(* The new implementation of utimes does not have the limitation of
   the standard one *)
let canSetTime f = true

(* We provide some kind of inode numbers *)
(* However, these inode numbers are not usable on FAT filesystems, as
   renaming a file "b" over a file "a" does not change the inode
   number of "a". *)
let hasInodeNumbers () = true

(****)

external getConsoleMode : unit -> int = "win_get_console_mode"
external setConsoleMode : int -> unit = "win_set_console_mode"
external getConsoleOutputCP : unit -> int = "win_get_console_output_cp"
external setConsoleOutputCP : int -> unit = "win_set_console_output_cp"

let terminalStateFunctions () =
  let oldstate = getConsoleMode () in
  let oldcp = getConsoleOutputCP () in
  (* Ctrl-C does not interrupt a call to ReadFile when
     ENABLE_LINE_INPUT is not set, so we handle Ctr-C
     as a character when reading from the console.
     We still want Ctrl-C to generate an exception when not reading
     from the console in order to be able to interrupt Unison at any
     time.  *)
  { defaultTerminal = (fun () -> setConsoleMode oldstate;
                                 setConsoleOutputCP oldcp);
    rawTerminal = (fun () -> setConsoleMode 0x19; setConsoleOutputCP 65001);
    startReading = (fun () -> setConsoleMode 0x18);
    stopReading = (fun () -> setConsoleMode 0x19) }
