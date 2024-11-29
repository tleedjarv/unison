#directory "+unix";;
#load "unix.cma";;

(* TODO:
     - Precedence rules? env should not precede over vars?
*)

module type TOOLS = sig
  module Env : Map.S with type key = string
  val env : string Env.t       (* Environment variables *)
  val args : string Env.t      (* Command line argument variables *)
  val inputs : string Env.t    (* [env] + [args], with [args] taking priority *)
  val vars : string Env.t ref  (* Internally defined variables *)
  val ( .$() ) : string Env.t -> string -> string (* Get value from any map *)
  val ( $ ) : string -> string (* Value from [vars] with fallback to [inputs] *)
  val ( <-- ) : string -> string -> unit  (* Set value in [vars] *)
  val ( <-+= ) : string -> string -> unit (* Append to value in [vars] (defaults to value in [inputs]) *)
  val ( <--? ) : string -> string -> string (* Copy from [inputs] to [vars], or set a default *)
  val shell : ?err_null:bool -> string -> string
  val get_command : string -> string option
  val is_empty : string -> bool
  val not_empty : string -> bool
  val exists : string -> string -> bool
  val info : string -> unit
  val error : string -> 'a
end
module Make = functor (Tools : TOOLS) -> struct
open Tools

(* TODO, improve these or not? Add a func? *)
let deps = ref []
let includes = ref []

(* [2023-05] This check is here only temporarily for a smooth transition *)
let () =
  if inputs.$("STATIC") = "true" then
    error "Variable STATIC is no longer in use. Please set appropriate \
      LDFLAGS, like -static, instead. See build instructions."

(*********************************************************************
*** Compilers ***)

let tool_prefix = "TOOL_PREFIX" <--?
  if inputs.$("OSCOMP") = "cross" then "x86_64-w64-mingw32-" else ""
  (* FIXME: OSCOMP is a legacy argument; probably not used by anyone *)

let ocamlc   = "OCAMLC"   <--? tool_prefix ^ "ocamlc"
let ocamlopt = "OCAMLOPT" <--? tool_prefix ^ "ocamlopt"

let ocaml_conf_var varname = shell (ocamlc ^ " -config-var " ^ varname)
(* Executing the compiler instead of getting the config from something
   like compiler-libs serves two purposes:
   - Getting the real target config: The target compiler may have
     different configuration than the one running this script;
   - Being more portable: compiler-libs is not guaranteed to be
     installed and available, even if the compiler itself is. *)

(*********************************************************************
*** Try to automatically guess OS ***)

(* Cygwin is for doing POSIX builds in Windows.
   MinGW is for native Windows builds in a minimal POSIX environment.
   MSVC is for native Windows builds without POSIX environment. *)

let osarch = "OSARCH" <--?
  match ocaml_conf_var "os_type" with
  | "Win32" -> "Win32"   (* Native Windows build *)
  | "Cygwin" -> "Cygwin" (* POSIX build for Windows *)
  | _ -> shell "uname"
(* Darwin is reported for macOS
   SunOS is reported for Solaris, OpenSolaris and derivatives (illumos) *)

let osarch_win32 = osarch = "Win32"
let osarch_cygwin = osarch = "Cygwin"
let osarch_macos = osarch = "Darwin"

(*********************************************************************
*** Try to automatically guess UI style ***)

let () =
  if inputs.$("UISTYLE") <> "" then
    error "UISTYLE is no longer used. See build instructions."

let ocaml_libdir = "OCAMLLIBDIR" <--? ocaml_conf_var "standard_library"

let ocamlfind = get_command "ocamlfind"

let build_GUI =
  let has_lablgtk3 =
    match ocamlfind with
    | Some cmd -> shell ~err_null:true (cmd ^ " query lablgtk3") |> not_empty
    | None -> exists ocaml_libdir "lablgtk3"
  in
  if not has_lablgtk3 then begin
    info "GUI library lablgtk not found. GTK GUI will not be built."
  end;
  has_lablgtk3
let () = if build_GUI then deps := "guimaybe: gui" :: !deps

let build_macGUI =
  if osarch_macos then begin
    (* If XCode is not installed, xcodebuild is just a placeholder telling
       that XCode is not installed and any invocation of xcodebuild results
       in a non-0 exit code. *)
    if Sys.command "xcodebuild -version > /dev/null" = 0 then
      true
    else begin
      info "Not building macOS native GUI because XCode is not installed.";
      false
    end
  end else begin
    info "Not on macOS. macOS native GUI will not be built.";
    false
  end
let () = if build_macGUI then deps := "macuimaybe: macui" :: !deps

(*********************************************************************
*** Default parameters ***)

(* Generate backtrace information for exceptions *)
let () = "CAMLFLAGS" <-+= "-g $(INCLFLAGS)"

let () =
  [
    "CAMLCFLAGS", "CFLAGS", "-ccopt";
    "CAMLCFLAGS", "CPPFLAGS", "-ccopt";
    "CAMLLDFLAGS", "LDFLAGS", "-cclib";
    "CLIBS", "LDLIBS", "-cclib";
  ]
  |> List.iter (fun (varname, envname, arg) ->
    if not_empty inputs.$(envname) then
      varname <-+= arg ^ " \"" ^ inputs.$(envname) ^ "\"")

(* The messy situation requiring the use of OUTPUT_SEL was fixed in OCaml 4.13.
   All usages of OUTPUT_SEL should be removed when 4.13 becomes a requirement. *)
let () =
  if osarch_win32 then begin
    (* Native Win32 build *)
    "EXEC_EXT" <-- ".exe";
    "OBJ_EXT" <-- ocaml_conf_var "ext_obj";
    if ocaml_conf_var "ccomp_type" = "msvc" then begin
      "OUTPUT_SEL" <-- "-Fo";
      "CLIBS" <-+= "-cclib user32.lib -cclib \"-link win32rc/unison.res\"";
      deps := "buildexecutable: win32rc/unison.res" :: !deps;
    end else begin
      "OUTPUT_SEL" <-- "-o";
      "CLIBS" <-+= "-cclib \"-link win32rc/unison.res.lib\"";
      deps := "buildexecutable: win32rc/unison.res.lib" :: !deps;
    end;
    "CAMLLDFLAGS_GUI" <-- "-cclib \"-subsystem windows\"";
    "CWD" <-- ".";
    "WINCOBJS" <-+= "system/system_win_stubs" ^ ($)"OBJ_EXT" ^ " lwt/lwt_unix_stubs" ^ ($)"OBJ_EXT";
    "WINOBJS" <-- "system/system_win.cmo";
    "SYSTEM" <-- "win";
    "building_for" <-- "Building for Windows";
  end else begin
    (* Unix build, or Cygwin POSIX (GNU C) build *)
    "OBJ_EXT" <-- ".o";
    "OUTPUT_SEL" <-- "'-o '";
    "WINOBJS" <-- "";
    "SYSTEM" <-- "generic";
    (* This is not strictly necessary as Cygwin can do a generic Unix build
       (and is actually meant to). *)
    if osarch_cygwin then begin
      "CWD" <-- ".";
      "EXEC_EXT" <-- ".exe";
      "CLIBS" <-+= "-cclib win32rc/unison.res.lib";
      deps := "buildexecutable: win32rc/unison.res.lib" :: !deps;
      "building_for" <-- "Building for Cygwin";
    end else begin
      "CWD" <-- shell "pwd";
      "EXEC_EXT" <-- "";
      (* openpty is in the libutil library *)
      if not osarch_macos && osarch <> "SunOS" then begin
        "CLIBS" <-+= "-cclib -lutil"
      end;
      if osarch = "SunOS" then begin
        (* ACL functions *)
        "CLIBS" <-+= "-cclib -lsec"
      end;
      "building_for" <-- "Building for Unix";
    end;
    deps := "manpage: manpagefile" :: !deps;
  end

(*********************************************************************
*** Compilation boilerplate ***)

let native =
  let native =
    let nat = ($)"NATIVE" |> String.lowercase_ascii in
    if nat <> "true" && nat <> "false" then
      get_command ocamlopt <> None
    else bool_of_string nat
  in
  "NATIVE" <-- string_of_bool native;
  native

let () =
  if native then begin
    (* Set up for native code compilation *)
    "CAMLC" <-- ocamlopt;
    "CAMLOBJS" <-- "$(OCAMLOBJS:.cmo=.cmx)";
    "CAMLOBJS_TUI" <-- "$(OCAMLOBJS_TUI:.cmo=.cmx)";
    "CAMLOBJS_GUI" <-- "$(OCAMLOBJS_GUI:.cmo=.cmx)";
    "CAMLOBJS_FSM" <-- "$(FSMOCAMLOBJS:.cmo=.cmx)";
    "CAMLOBJS_MAC" <-- "$(OCAMLOBJS_MAC:.cmo=.cmx)";
    "CAMLLIBS" <-- "$(OCAMLLIBS:.cma=.cmxa)";
    "CAMLLIBS_TUI" <-- "$(OCAMLLIBS_TUI:.cma=.cmxa)";
    "CAMLLIBS_GUI" <-- "$(OCAMLLIBS_GUI:.cma=.cmxa)";
    "CAMLLIBS_MAC" <-- "$(OCAMLLIBS_MAC:.cma=.cmxa)";
    "CAMLLIBS_FSM" <-- "$(FSMOCAMLLIBS:.cma=.cmxa)";
  end else begin
    (* Set up for bytecode compilation *)
    "CAMLC" <-- ocamlc;
    (* -output-complete-exe is available since OCaml 4.10
       OCaml > 5.2.0 no longer supports detection of compiler options,
       hence the hack of comparing the output to -version. *)
    if String.trim (shell (ocamlc ^ " -output-complete-exe -version 2>&1")) =
       String.trim (shell (ocamlc ^ " -version")) then begin
      "CAMLLDFLAGS" <-+= "-output-complete-exe";  (* can safely strip the binary *)
    end else begin
      "CAMLLDFLAGS" <-+= "-custom";
    end;
    "CAMLOBJS" <-- "$(OCAMLOBJS)";
    "CAMLOBJS_TUI" <-- "$(OCAMLOBJS_TUI)";
    "CAMLOBJS_GUI" <-- "$(OCAMLOBJS_GUI)";
    "CAMLOBJS_MAC" <-- "$(OCAMLOBJS_MAC)";
    "CAMLOBJS_FSM" <-- "$(FSMOCAMLOBJS)";
    "CAMLLIBS" <-- "$(OCAMLLIBS)";
    "CAMLLIBS_TUI" <-- "$(OCAMLLIBS_TUI)";
    "CAMLLIBS_GUI" <-- "$(OCAMLLIBS_GUI)";
    "CAMLLIBS_MAC" <-- "$(OCAMLLIBS_MAC)";
    "CAMLLIBS_FSM" <-- "$(FSMOCAMLLIBS)";
  end

let () = "WINDRES" <--
  (if not_empty tool_prefix then tool_prefix else begin
    let cc = Filename.basename (ocaml_conf_var "c_compiler") in
    let rec extract_prefix pre l =
      match l with
      | [] -> "" (* could not detect the prefix *)
      | x :: xs ->
          let found_base =
            String.length x > 1 && String.(sub x 0 2 |> lowercase_ascii) = "cl" ||
            String.length x > 2 && String.(sub x 0 3 |> lowercase_ascii) = "gcc"
          in
          if not found_base then extract_prefix (x :: pre) xs
          else if pre = [] then "" else (String.concat "-" (List.rev pre)) ^ "-"
    in
    extract_prefix [] (String.split_on_char '-' cc)
  end) ^ "windres"

let () =
  if is_empty inputs.$("_NMAKE_VER") then
    "ALL__SRC" <-- "$^$>"  (* First one is for GNU and POSIX make, the other for BSD make *)
  else
    "ALL__SRC" <-- "$(**)" (* NMAKE; enclose in brackets for safety if not run by NMAKE *)

let () = "rule_sep" <-- if not_empty inputs.$("ASSUME_DMAKE") then ":=" else ":"

(*********************************************************************
*** User Interface setup ***)

let () = "CAMLFLAGS_GUI" <-+=
  match ocamlfind with
  | Some cmd ->
      (* The weird quoting is required for Windows, but harmless in sh *)
      shell (cmd ^ " query -format \"-I \"\"%d\"\"\" lablgtk3") ^ " " ^
      shell (cmd ^ " query -format \"-I \"\"%d\"\"\" cairo2")
  | None -> "-I +lablgtk3 -I +cairo2"

let () =
  if osarch_macos && is_empty inputs.$("XCODEFLAGS") then
    (* Prevent Xcode from trying to build universal binaries by default *)
    "XCODEFLAGS" <-- "-arch " ^ (shell "uname -m")

(*********************************************************************
*** Filesystem monitoring ***)

let fsmonitor_dir =
  match osarch with
  | "Linux" -> "FSMDIR" <--? "fsmonitor/inotify"
  | "SunOS" -> "FSMDIR" <--? "fsmonitor/solaris"
  | "Win32" -> "FSMDIR" <--? "fsmonitor/windows"
  | "FreeBSD" | "OpenBSD" | "NetBSD" | "DragonFly" ->
      begin
        let libinotify_lib = "-cclib '" ^ (shell "pkg-config --libs libinotify 2> /dev/null || printf ' -linotify'") ^ "'" in
        let libinotify_inc = "-ccopt '" ^ (shell "pkg-config --cflags libinotify 2> /dev/null") ^ "'" in
        if shell ("{ printf '' > inotifytest__.ml ;" ^ ocamlc ^ " " ^
          ($)"CAMLCFLAGS" ^ " " ^ ($)"CAMLLDFLAGS" ^ " " ^ libinotify_lib ^
          " -o inotifytest__ inotifytest__.ml > /dev/null 2>&1 && printf true ; } ;\
            rm -f inotifytest__.ml inotifytest__.cm[oix] \
            inotifytest__.o inotifytest__ > /dev/null 2>&1") = "true" then begin
          includes := ("$(NAME_FSM)$(EXEC_EXT): CAMLCFLAGS += " ^ libinotify_inc) :: !includes;
          includes := ("$(NAME_FSM)$(EXEC_EXT): CLIBS += " ^ libinotify_lib) :: !includes;
          "FSMDIR" <--? "fsmonitor/inotify"
        end else
          inputs.$("FSMDIR")
      end
  | _ -> inputs.$("FSMDIR")

let build_fsmonitor =
  if not_empty fsmonitor_dir then begin
    includes := ("include " ^ fsmonitor_dir ^ "/Makefile") :: !includes;
    true
  end else begin
    info "fsmonitor implementation is not available or not configured for this system. fsmonitor will not be built.";
    false
  end
let () = if build_fsmonitor then deps := "fsmonitor: fsmonitorexecutable" :: !deps


(*let () = "RM" <-- "ocaml " ^ (Filename.concat (Sys.getcwd ()) Sys.argv.(0)) ^ " rm"*)
let () = "RM" <-- if is_empty inputs.$("_NMAKE_VER") then "rm -f" else "del /f"

let () =
  Env.iter (fun k v -> print_string k; print_string " = "; print_endline v) !vars

let () =
  List.iter print_endline !includes

let () =
  List.iter print_endline !deps

let () =
  info "";
  info (($)"building_for");
  info ("NATIVE = " ^ ($)"NATIVE");
  info ""

end


module Tools = struct

module Env = Map.Make(String)

let ( .$() ) m n =
  try Env.find n m with Not_found ->
    try
      Env.filter
        (fun k _ -> String.(equal (uppercase_ascii n) (uppercase_ascii k))) m
      |> Env.choose |> snd
    with Not_found -> ""

let ( .%() ) m n =
  try Some (Env.find n m) with Not_found ->
    try
      Some (Env.filter
          (fun k _ -> String.(equal (uppercase_ascii n) (uppercase_ascii k))) m
        |> Env.choose |> snd)
    with Not_found -> None

let make_env arr =
  let add_var map s =
    let k, v =
      try
        let open String in
        let v = index s '=' in
        sub s 0 v,
        try sub s (v + 1) (length s - v - 1) with Invalid_argument _ -> ""
      with Not_found -> s, ""
    in
    Env.add k v map
  in
  Array.fold_left add_var Env.empty arr

let env =
  Unix.handle_unix_error Unix.environment () |> make_env

let args =
  (if Array.length Sys.argv < 2 then [||] else
    Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
  |> make_env

let inputs =
  let override _ a b =
    match a, b with
    | Some _, None -> a
    | _, Some _ -> b
    | None, None -> None
  in
  Env.merge override env args

let vars = ref Env.empty

let ( $ ) k =
  match !vars.%(k) with Some s -> s | None -> inputs.$(k)

let ( <-- ) k v =
  vars := Env.add k v !vars

let ( <-+= ) k v =
  vars := Env.add k (($)k ^ " " ^ v) !vars

let ( <--? ) k v =
  match !vars.%(k) with
  | Some s -> s
  | None -> begin
      match inputs.%(k) with
      | Some s -> vars := Env.add k s !vars; s
      | None -> vars := Env.add k v !vars; v
      end

let shell ?(err_null = false) cmd =
  let shell' open_proc close_proc =
    let outp = open_proc () in
    let buf = Buffer.create 512 in
    let () = try Buffer.add_channel buf outp 16384 with | End_of_file -> () in
    (* Trim the final newline *)
    let trim_end =
      if Buffer.length buf >= 2 then begin
        if Buffer.(sub buf (length buf - 2) 2) = "\r\n" then 2
        else if Buffer.(sub buf (length buf - 1) 1) = "\n" then 1
        else 0
      end else if Buffer.length buf >= 1 && Buffer.(sub buf (length buf - 1) 1) = "\n" then 1
      else 0
    in
    match close_proc () with
    | _ -> Buffer.(sub buf 0 (length buf - trim_end))
  in
  Unix.handle_unix_error (fun () ->
    if err_null then
      let (sh_out, sh_in, _) as sh_full = Unix.open_process_full cmd [||] in
      let () = close_out sh_in in
      shell' (fun () -> sh_out) (fun () -> Unix.close_process_full sh_full)
    else
      let sh_out = Unix.open_process_in cmd in
      shell' (fun () -> sh_out) (fun () -> Unix.close_process_in sh_out)
  ) ()

let path =
  env.$("PATH")
  |> String.split_on_char (if Sys.win32 then ';' else ':')

let pathext =
  if Sys.win32 || Sys.cygwin then
    env.$("PATHEXT") |> String.split_on_char ';'
  else
    [""]

let search_in_path ?(path = path) name =
  Filename.concat
    (List.find
      (fun dir -> List.exists
        (fun ext -> Sys.file_exists (Filename.concat dir (name ^ ext)))
        pathext)
      path)
    name

let search_in_path_opt ?path name =
  try Some (search_in_path ?path name) with
  | Not_found -> None

let get_command name = search_in_path_opt name

let exists dir name =
  Sys.file_exists (Filename.concat dir name)

let rm =
  let rm' n =
    if String.length n > 0 && n.[0] <> '-' && n.[0] <> '/' && n.[0] <> '\\'
        && not (String.contains n '*') then
      try Sys.remove n with Sys_error _ -> ()
  in
  Array.iter rm'

let is_empty s =
  String.length (String.trim s) = 0

let not_empty s = not (is_empty s)

let info msg =
  prerr_endline msg

let error msg =
  prerr_string "Error: ";
  prerr_endline msg;
  exit 2

end


let target_local_vars () =
  let open Tools in
  if is_empty inputs.$("_NMAKE_VER") then
    print_string
      {mk|
$(NAME_GUI)$(EXEC_EXT) $(CAMLOBJS_GUI) $(rule_sep) CAMLFLAGS_GUI_X = $(CAMLFLAGS_GUI)

$(NAME_FSM)$(EXEC_EXT) $(CAMLOBJS_FSM) $(FSMOCAMLOBJS:.cmo=.cmi) $(rule_sep) CAMLFLAGS_FSM_X = $(CAMLFLAGS_FSM)
|mk}
  else
    print_string
      {mk|
CAMLFLAGS_GUI_X = $(CAMLFLAGS_GUI)
CAMLFLAGS_FSM_X = $(CAMLFLAGS_FSM)
|mk}


let install () =
  let open Tools in
  if not_empty inputs.$("_NMAKE_VER") || Sys.file_exists "src/unison.exe" then begin
    let cwd = Sys.getcwd () in
    let map_sep =
      if String.contains cwd '\\' then function '/' -> '\\' | c -> c
      else fun c -> c
    in
    let files =
      let check_file name l =
        if Sys.file_exists name then
          (String.map map_sep (Filename.concat cwd name)) :: l
        else l
      in
      List.fold_right check_file
        ["src/unison.exe"; "src/unison-gui.exe"; "src/unison-fsmonitor.exe"]
        []
    in
    if files = [] then
      error "The application has not been built yet."
    else
      info ("\n\nYou can find the built application as the following files \
        that you can copy to your desired destination.\n    " ^
        (String.concat "\n    " files) ^ "\n");
      exit 0
  end;

  let install = "INSTALL" <--? "install" in
  let install_program = "INSTALL_PROGRAM" <--? install in
  let install_data = "INSTALL_DATA" <--? install ^ " -m 644" in

  let destdir = ($)"DESTDIR" in
  let prefix = "PREFIX" <--? "/usr/local" in
  let exec_prefix = "EXEC_PREFIX" <--? prefix in
  let bindir = "BINDIR" <--? exec_prefix ^ "/bin" in
  let datarootdir = "DATAROOTDIR" <--? prefix ^ "/share" in
  let mandir = "MANDIR" <--? datarootdir ^ "/man" in
  let man1dir = "MAN1DIR" <--? mandir ^ "/man1" in
  let manext = "MANEXT" <--? ".1" in

  let exec cmd =
    let cmd = String.concat " " cmd in
    print_endline cmd;
    match Sys.command cmd with
    | 0 -> ()
    | e -> error (string_of_int e)
  in
  let install_if_exists dir name dest =
    if exists dir name then exec
      [install_program; Filename.concat dir name; Filename.concat dest name]
  in

  print_endline ("DESTDIR = " ^ destdir);
  print_endline ("PREFIX = " ^ prefix);
  exec [install; "-d"; destdir ^ bindir];
  install_if_exists "src" "unison" (destdir ^ bindir);
  install_if_exists "src" "unison-gui" (destdir ^ bindir);
  install_if_exists "src" "unison-fsmonitor" (destdir ^ bindir);
  if not (exists "src" "unison-fsmonitor") then begin
    (* FIXME: fsmonitor.py is legacy and unmaintained. Drop? *)
    install_if_exists "src" "fsmonitor.py" (destdir ^ bindir)
  end;
  if exists "man" "unison.1" then begin
    exec [install; "-d"; destdir ^ man1dir];
    exec [install_data; "man/unison.1"; destdir ^ man1dir ^ "/unison" ^ manext]
  end;
  if exists "src/uimac/build/Default" "Unison.app" then begin
    print_endline ("!!! The GUI for macOS has been built but will NOT be \
      installed automatically. You can find the built GUI package at " ^
      (Filename.concat (Sys.getcwd ()) "src/uimac/build/Default/Unison.app"))
  end

let () =
  if Array.length Sys.argv < 2 then
    Tools.error "Missing sub-command"
  else
    match Sys.argv.(1) with
    | "conf" -> let module Conf = Make(Tools) in ()
    | "conf2" -> target_local_vars ()
    | "install" -> install ()
    | "rm" -> Tools.rm (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
    | s -> Tools.error ("Invalid sub-commmand '" ^ s ^ "'")
