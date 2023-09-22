(* Unison file synchronizer: src/fingerprint.ml *)
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

(* NOTE: IF YOU CHANGE TYPE "FINGERPRINT", THE ARCHIVE FORMAT CHANGES;       *)
(* INCREMENT "UPDATE.ARCHIVEFORMAT"                                          *)
type t = string

let m = Umarshal.string

let pseudo_prefix = "LEN"

let pseudo path len = pseudo_prefix ^ (Uutil.Filesize.toString len) ^ "@" ^
                      (Digest.string (Path.toString path))

let ispseudo f = Util.startswith f pseudo_prefix

type algorithm = MD5 | XXH3_64

let featAlgos =
  Features.register "Fingerprint algorithms" ~arcFormatChange:false None

let internAlgo = function
  | "MD5" -> MD5
  | "XXH3_64" -> XXH3_64
  | s -> raise (Prefs.IllegalValue
        ("Invalid fingerprinting algorithm requested: " ^ s))

let externAlgo = function
  | MD5 -> "MD5"
  | XXH3_64 -> "XXH3_64"

let algopref =
  Prefs.create "fpalgo" MD5 (* Default for legacy versions *)
    ~category:(`Internal `Pseudo)
    ~send:(fun () -> Features.enabled featAlgos)
    "*Pseudo-preference for internal use only" ""
    (fun _ -> internAlgo)
    (fun a -> [externAlgo a])
    Umarshal.(sum1 string externAlgo internAlgo)

let algoprefUser =
  Prefs.create "fpalgorithm" None
    ~category:(`Expert)
    ~local:true
    ~send:(fun () -> false)
    "file fingerprinting algorithm"
    "Select the algorithm that is used for fingerprinting file contents to \
    detect changes and verify sync results. Normally, you should not need to \
    set this preference manually; an optimal algorithm is automatically \
    selected between client and server. Currently supported algorithms are: \
    {\\tt MD5} - for legacy compatibility; \
    {\\tt XXH3\\_64} - xxHash XXH3 64 bits (the default when both client and \
    server support it)."
    (fun _ -> function
      | "" -> None | a -> Some (internAlgo String.(trim a |> uppercase_ascii)))
    (function None -> [""] | Some a -> [externAlgo a])
    Umarshal.(option (sum1 string externAlgo internAlgo))

let registerAlgo featrName algo =
  Features.register ("Fingerprint: " ^ featrName) ~arcFormatChange:false
    (Some (fun _ featrEnabled ->
      if not featrEnabled && Prefs.read algoprefUser = Some algo then
        Some "The requested fingerprinting algorithm (the \
          \"fpalgorithm\" preference) is not supported by the server."
      else None))

let algoXXH3_64 = registerAlgo "XXH3_64" XXH3_64

(* Highest priority first in list. Not all algorithms have to be in this list;
   if not in list then it will never be selected automatically (except for the
   legacy MD5). *)
let defaultAlgoPriority = [
    ((fun () -> Features.enabled algoXXH3_64), XXH3_64);
  ]

let init () =
  (* Default to the highest priority algorithm supported by (and enabled on)
     both client and server. *)
  let rec setFirstActive = function
    | [] -> ()
    | (active, p) :: _ when active () -> Prefs.set algopref p
    | _ :: xs -> setFirstActive xs
  in
  match Prefs.read algoprefUser with
  | None -> setFirstActive defaultAlgoPriority
  | Some a -> Prefs.set algopref a

let algo_funcs = function
  | MD5 -> ("", Digest.channel)
  | XXH3_64 -> ("\001", Xxhash.XXH3_64.channel)

let active_algo () = Prefs.read algopref

let algo h =
  let len = String.length h in
  if len = 0 then active_algo () (* Dummy fingerprint *)
  else match len, String.unsafe_get h 0 with
  | 9, '\001' -> XXH3_64
  | 16, _ -> MD5 (* Algorithm is not embedded in the fingerprint, this means
                    it is MD5. For compatibility with versions which always
                    used the same algorithm (<= 2.53.4). All algorithms added
                    later have the algorithm tag embedded in the fingerprint
                    as the first byte. This means that no fingerprint value
                    with an embedded tag will be exactly 16 bytes (128 bits)
                    in length. *)
  | _ ->
      if ispseudo h then active_algo ()
      else assert false

let with_algo ?(algoOf = "") f =
  let alg = algo algoOf in
  let (prefix, funcs) = algo_funcs alg in
  prefix ^ f funcs

let same_algo h1 h2 =
  algo h1 = algo h2

let has_active_algo h =
  algo h = active_algo ()

(* Assumes that (fspath, path) is a file and gives its ``digest '', that is  *)
(* a short string of cryptographic quality representing it.                  *)
let file_aux digestChannel fspath path =
  let f = Fspath.concat fspath path in
  Util.convertUnixErrorsToTransient
    ("digesting " ^ Fspath.toPrintString f)
    (fun () ->
       let ic = Fs.open_in_bin f in
       try
         let d = digestChannel ic (-1) in
         close_in ic;
         d
       with e ->
         close_in_noerr ic;
         raise e
    )

let file ?algoOf fspath path =
  with_algo ?algoOf (fun algo_funcs -> file_aux algo_funcs fspath path)

let maxLength = Uutil.Filesize.ofInt max_int
let subfile_aux digestChannel path offset len =
  if len > maxLength then
    raise (Util.Transient
             (Format.sprintf "File '%s' too big for fingerprinting"
                (Fspath.toPrintString path)));
  Util.convertUnixErrorsToTransient
    "digesting subfile"
    (fun () ->
       let inch = Fs.open_in_bin path in
       begin try
         LargeFile.seek_in inch offset;
         let res = digestChannel inch (Uutil.Filesize.toInt len) in
         close_in inch;
         res
       with
         End_of_file ->
           close_in_noerr inch;
           raise (Util.Transient
                    (Format.sprintf
                       "Error in digesting subfile '%s': truncated file"
                       (Fspath.toPrintString path)))
       | e ->
           close_in_noerr inch;
           raise e
       end)

let subfile ?algoOf path offset len =
  with_algo ?algoOf (fun algo_funcs -> subfile_aux algo_funcs path offset len)

let int2hexa quartet =
  if quartet < 10 then
    (char_of_int ((int_of_char '0') + quartet))
  else char_of_int ((int_of_char 'a') + quartet - 10)

let hexaCode theChar =
  let intCode = int_of_char theChar in
  let first = intCode / 16 in
  let second = intCode mod 16 in
  (int2hexa first, int2hexa second)

let toString md5 =
  if ispseudo md5 then md5 else begin
    let length = String.length md5 in
    let string = Bytes.create (length * 2) in
    for i=0 to (length - 1) do
      let c1, c2 =  hexaCode (md5.[i]) in
      Bytes.set string (2*i) c1;
      Bytes.set string (2*i + 1) c2;
    done;
    Bytes.to_string string
  end

let dummy = ""

let hash d =
  let l = String.length d in
  if l = 0 then
    1234577
  else begin
    assert (l >= 3);
    Char.code (String.unsafe_get d 0) +
    (Char.code (String.unsafe_get d 1) lsl 8) +
    (Char.code (String.unsafe_get d 2) lsl 16)
  end

let equal (d : string) d' = d = d'
