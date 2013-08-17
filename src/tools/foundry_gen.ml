let load_ir lexbuf =
  let lex () = IrLexer.next lexbuf in
  let parse  = MenhirLib.Convert.Simplified.traditional2revised IrParser.toplevel in
  parse lex

let _ =
  let output = ref ""   in
  let force  = ref false in
  let dump   = ref false in
  let inputs = ref []    in

  Arg.parse (Arg.align [
      "-o", Arg.Set_string output,
        "<file> Output file";
      "-f", Arg.Set force,
        " Write LLVM bitcode to a tty";
      "-dump", Arg.Set dump,
        " Dump LLVM IR to stderr"
    ]) (fun arg ->
      inputs := arg :: !inputs)
    ("Usage: " ^ (Sys.argv.(0) ^ " [options] <input-file>..."));

  let input_ir =
    Unicode.Std.String.concat u""
      (List.map Io.input_all
        (List.map Io.open_in !inputs)) in

  let roots, capsule = load_ir (Lexing.from_string (input_ir :> string)) in
  let llmod = Codegen.llvm_module_of_ssa_func (Ssa.find_func u"main" capsule) in

  if !dump then
    Llvm.dump_module llmod;

  if !output <> "" then
    let chan = Io.open_out !output in
    if !force || not (Unix.isatty (Unix.descr_of_out_channel chan)) then begin
      if not (Llvm_bitwriter.output_bitcode ~unbuffered:true chan llmod) then
        prerr_endline ("Cannot write bitcode to \"" ^ !output ^ "\"")
    end else begin
      prerr_endline "Refusing to output LLVM bitcode to a tty.";
      prerr_endline "Pass -f if this is what you actually want."
    end
