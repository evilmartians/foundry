let dump out udb =
  output_string out "\
open Codepoint

let data = [|
";
  List.iter (fun cp -> Printf.fprintf out "  %s;\n" (Udb_print.string_of_codepoint cp)) udb;
  output_string out "|]\n"


let dump out udb =
  let data = Marshal.(to_string (Array.of_list udb) []) in
  Printf.fprintf out "\
open Codepoint

let data : Codepoint.t array = Marshal.from_string \"%s\" 0
" (String.escaped data)


let csv out udb =
  List.iter (fun cp -> Printf.fprintf out "%s\n" (Udb_print.csv_of_codepoint cp)) udb


let () =
  if Array.length Sys.argv = 3 then
    let infile = Sys.argv.(1) in
    let outfile = Sys.argv.(2) in
    let ucd = Udb_lexer.codepoint_list (Lexing.from_channel (open_in infile)) in
    let out = open_out outfile in
    dump out ucd;
    close_out out
  else
    UcsTest.test ()
