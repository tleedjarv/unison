(* Unison file synchronizer: src/ugettext/ugettext_sys.ml *)
(* Copyright 2026, Tõivo Leedjärv

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

module Ugettext_Config = struct
  let textdomain = "unison"
  let codeset = None (* Allow system gettext to decide *)
  let dir =
    (* System gettext does not search for MO files in multiple dirs.
       caml-gettext-stub does not search for MO files either, unlike
       the pure OCaml implementation, so we have to do the search here.
       First try the path prefix of the executable, just like with the
       pure OCaml backend. *)
    let mo = textdomain ^ ".mo" in
    let exe =
      if Filename.is_relative Sys.executable_name then
        Filename.concat (Sys.getcwd ()) Sys.executable_name
      else
        Sys.executable_name in
    let bindir = Filename.dirname exe in
    let rootdir = Filename.concat bindir Filename.parent_dir_name in
    let sharedir = Filename.concat rootdir "share" in
    let localedir = Filename.concat sharedir "locale" in
    (* Create a dummy throw-away Gettext module. It is only needed temporarily
       to use GettextLocale.Posix and to get access to the list of built-in
       default paths. *)
    let t = GettextModules.create ~failsafe:Ignore ?codeset textdomain in
    let (search_locales, _codeset) =
      GettextLocale.Posix.get_locale t GettextCategory.LC_MESSAGES in
    let search_dirs = localedir :: t.path in
    let found_dir =
      search_dirs |> List.find_opt
        (fun dir ->
          search_locales |> List.exists
            (fun locale ->
              let fn =
                Filename.concat (Filename.concat (Filename.concat dir locale)
                    "LC_MESSAGES") mo in
              Sys.file_exists fn))
    in
    match found_dir with
    | Some _ -> found_dir
    | None -> Some GettextConfig.default_dir
  let dependencies = Gettext.init
end

include Gettext.Program (Ugettext_Config) (GettextStub.Native)

let gettext_argspecs = fst init
