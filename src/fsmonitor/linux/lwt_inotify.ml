(* Unison file synchronizer: src/monitoring-linux/lwt_inotify.ml *)
(* Copyright 1999-2012, Benjamin C. Pierce

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

let (>>=) = Lwt.bind

type t =
  { fd : Unix.file_descr;
    lwt_fd : Lwt_unix.file_descr;
  }

let init () =
  let fd = Inotify.init () in
  { fd = fd;
    lwt_fd =
      Lwt_unix.of_unix_file_descr (*~blocking:false ~set_flags:true*) fd;
  }

let add_watch st path sel =
(*  Lwt_unix.check_descriptor st.lwt_fd;*)
  Inotify.add_watch st.fd path sel

let rm_watch st wd =
(*  Lwt_unix.check_descriptor st.lwt_fd;*)
  Inotify.rm_watch st.fd wd

let rec read st =
  Lwt_unix.wait_read st.lwt_fd >>= fun () ->
  try Lwt.return (Inotify.read st.fd) with Unix.Unix_error (EAGAIN, _, _) -> read st

let close st = Lwt_unix.close st.lwt_fd
