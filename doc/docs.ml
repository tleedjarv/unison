open Printf

let main() = begin

(* The structure *)
set_binary_mode_out stdout true;
let ml = stdout in
fprintf ml "(* DO NOT MODIFY.\n\
\032  This file has been automatically generated, see docs.ml. *)\n\n";

(* Process the manual *)
let rec findFirstSNIP ch =
  try
    let l = input_line ch in
    if l <> "----SNIP----" then findFirstSNIP ch
  with
    End_of_file ->
      (Printf.printf "File does not contain ----SNIP----\n";
       exit 1) in

let transl_comment =
  "(* TRANSLATORS: Section heading for manual in UI *)" in

let transl_comment2 =
  "(* TRANSLATORS: This is auto-generated for the manual in UI.\n\
    Please judge if translation is needed or not. *)" in

let should_translate name s =
  match name with
  | "Junk" -> ""
  | _ -> s ^ "s_ "
in

let prsection ch =
  let name = input_line ch in
  let shortname = input_line ch in
  if shortname <> "" then begin
    let empty = input_line ch in
    if empty<>"" then
      (fprintf stderr "Second line after SNIP is '%s', not empty!\n" empty;
       exit 1)
    end;
  fprintf ml "    (\"%s\", (%s\"%s\", \n     %s\""
    shortname (should_translate name transl_comment) name (should_translate name transl_comment2);
  let rec loop () =
    let l = input_line ch in
    if l<>"----SNIP----" then begin
      for n=0 to (String.length l) - 1 do
        let e =
          if n=0 && l.[n]=' ' then "\\032"
          else if l.[n]='"' then "\\\""
          else if l.[n]='\'' then "'"
          else if (Char.code l.[n])>=128 then sprintf "\\%d" (Char.code l.[n])
          else Char.escaped l.[n] in
        output_string ml e;
      done;
      fprintf ml "\\n\\\n      ";
      loop()
    end in
  (try loop() with End_of_file -> ());
  fprintf ml "\"))\n::\n" in

let prmanual() =
  fprintf ml "let docs =\n";
  set_binary_mode_in stdin true;
  let ch = stdin in
  findFirstSNIP ch;
  try
    while true do prsection ch done
  with End_of_file -> ();
  fprintf ml "    [];;\n\n" in

(* Docs *)
prmanual ();

end (* of main *);;
(*--------------------------------------------------------------------------*)

Printexc.catch main ();;
