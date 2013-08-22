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
    ("Usage: " ^ Sys.argv.(0) ^ " [options] <input-file>...");

  if !inputs = [] then
    inputs := ["-"];

  let input_ir =
    Unicode.Std.String.concat u""
      (List.map Io.input_all
        (List.map Io.open_in !inputs)) in

  let roots, capsule = load_ir (Lexing.from_string (input_ir :> string)) in
  Rt.roots := roots;

  let llmod = Codegen.llvm_module_of_ssa_capsule capsule in

  (* cortex-m3 *)
  Llvm.set_target_triple "thumbv7m-none--eabi" llmod;
  Llvm.set_data_layout   "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32-S64" llmod;

  (* x86_64 *)
  Llvm.set_target_triple "x86_64-pc-linux-gnu" llmod;
  Llvm.set_data_layout   "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128" llmod;

  if !dump then
    Llvm.dump_module llmod
  else if !output = "" then
    output := "-";

  if !output <> "" then
    let chan = Io.open_out !output in
    if !force || not (Unix.isatty (Unix.descr_of_out_channel chan)) then begin
      if not (Llvm_bitwriter.output_bitcode ~unbuffered:true chan llmod) then
        prerr_endline ("Cannot write bitcode to \"" ^ !output ^ "\"")
    end else begin
      prerr_endline "Refusing to output LLVM bitcode to a tty.";
      prerr_endline "Pass -f if this is what you actually want."
    end
