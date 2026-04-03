(* Unison file synchronizer: src/ugettext/ugettext_ocaml.ml *)
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
  let codeset = Some "UTF-8"
  let dir =
    (* For locale dir, prefer the prefix from the path of the executable.
       This way, when an executable is /usr/pkg/bin/unison, for example, then
       /usr/pkg/share/locale is tried first. This also works in Windows, unlike
       ocaml-gettext default dirs.
       If unison.mo is not found here then ocaml-gettext will additionally try
       its own list of configured defaults. *)
    let exe =
      if Filename.is_relative Sys.executable_name then
        Filename.concat (Sys.getcwd ()) Sys.executable_name
      else
        Sys.executable_name in
    let bindir = Filename.dirname exe in
    let rootdir = Filename.concat bindir Filename.parent_dir_name in
    let sharedir = Filename.concat rootdir "share" in
    let localedir = Filename.concat sharedir "locale" in
    Some localedir
  let dependencies = Gettext.init
end

module UgettextUTF8 = struct
  (* By supporting UTF-8 only (which includes US-ASCII), we avoid the depdency
     on Camomile that ocaml-gettext would otherwise have. *)
  module Charset_UTF8Only = struct
      type encoding = string
      type u = unit

      let create _t in_enc out_enc =
        if String.uppercase_ascii in_enc <> "UTF-8" then
          failwith ("gettext: Invalid IN encoding: " ^ in_enc ^
            "; only UTF-8 is supported")
        else if out_enc <> "" && String.uppercase_ascii out_enc <> "UTF-8" then
          failwith ("gettext: Invalid OUT encoding " ^ out_enc ^
            "; only UTF-8 is supported")
        else ()

      let recode () str = str
  end

  module Map =
    GettextRealize.Generic
      (GettextTranslate.Map) (Charset_UTF8Only) (GettextLocale.Posix)
end

include Gettext.Program (Ugettext_Config) (UgettextUTF8.Map)
