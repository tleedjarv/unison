(* Unison file synchronizer: src/compat.ml *)
(* Copyright 2022, Tõivo Leedjärv

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

let archive202205 =
  Features.register "Archive: v2022-05" ~arcFormatChange:true None

let isArchBefore202205 () = not (Features.enabled archive202205)

let isArchAfter202205 () = Features.enabled archive202205
