open Unicode.Std

let load_ir lexbuf =
  let lex () = IrLexer.next lexbuf in
  let parse  = MenhirLib.Convert.Simplified.traditional2revised IrParser.toplevel in
  let roots, main = parse lex in
    Rt.roots := roots;
    main

let dump_ir main =
  IrPrinter.print_ssa main

let _ =
  let main = load_ir (Lexing.from_channel stdin) in
  let llmod = Codegen.llvm_module_of_ssa_func main in
  if (Array.length Sys.argv) > 1 then
    ignore (if Sys.argv.(1) = ("-" :> latin1s) then
      Llvm_bitwriter.output_bitcode ~unbuffered:true stdout llmod
    else
      Llvm_bitwriter.write_bitcode_file llmod Sys.argv.(1))

(* let env = Vm.env_create ()

while true do
  let lexbuf   = Ulexing.from_utf8_channel stdin in
  let lexstate = Lexer.create "stdin" 1 in
  let lex ()   = Lexer.next lexstate lexbuf in
  let parse    = MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel in

(* let print_stmt stmt =
    stmt
    |> Syntax.sexp_of_expr
    |> Sexp.to_string_hum
    |> print_endline
  in List.iter (Parser.toplevel lex lexbuf) ~f:print_stmt
*)

  try
    ignore (Vm.eval env (parse lex));
    print_endline (IrPrinter.print_roots !Rt.roots)
  with Rt.Exc exc ->
(*     let pointers =
      let all_ranges = exc.Vm.ex_location :: exc.Vm.ex_highlights in
        String.make (List.fold ~f:max ~init:0 (List.map all_ranges ~f:snd) + 1) ' '
    in
      List.iter ~f:(fun (x, y) -> String.fill pointers x (y - x) '~') exc.Vm.ex_highlights;
      (let x, y = exc.Vm.ex_location in
        String.fill pointers x (y - x) '^');

      print_endline pointers;
 *)
      print_endline (Location.at (List.hd exc.Rt.ex_locations));
      print_endline ("Error: " ^ exc.Rt.ex_message)

done
 *)
