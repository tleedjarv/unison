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
  let dir = Some GettextConfig.default_dir
  let dependencies = Gettext.init
end

include Gettext.Program (Ugettext_Config) (GettextStub.Native)
