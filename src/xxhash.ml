(* Unison file synchronizer: src/xxhash.ml *)
(* Copyright 2023, Tõivo Leedjärv

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

module type XXH = sig
  type t = string
  val channel : in_channel -> int -> t
end

module XXH3_64 : XXH = struct
  type t = string
  external channel : in_channel -> int -> t = "unsn_xxh3_64_chan"
end
