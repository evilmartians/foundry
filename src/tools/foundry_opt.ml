let load_ir lexbuf =
  let lex () = IrLexer.next lexbuf in
  let parse  = MenhirLib.Convert.Simplified.traditional2revised IrParser.toplevel in
  parse lex

let dump_ir ?roots capsule =
  IrPrinter.string_of ?roots capsule

let _ =
  let output   = ref "-"   in
  let no_roots = ref false in
  let inputs   = ref []    in
  let xfrms   = ref []    in

  let append_xfrm xfrm () =
    xfrms := xfrm :: !xfrms
  in

  Arg.parse (Arg.align [
      "-o", Arg.Set_string output,
        "<file> Output file";

      "-ordered", Arg.Set IrPrinter.ordered,
        " Iterate symbol tables in alphabetical order";

      "-no-roots", Arg.Set no_roots,
        " Don't print out root data structures";

      "-dce", Arg.Unit (append_xfrm Dead_code_elim.run_on_capsule),
        " Dead Code Elimination";

      "-simplify-cfg", Arg.Unit (append_xfrm Simplify_cfg.run_on_capsule),
        " CFG Simplification";

      "-simplify-frames", Arg.Unit (append_xfrm Simplify_frames.run_on_capsule),
        " Frame Simplification";

      "-infer", Arg.Unit (append_xfrm Local_inference.run_on_capsule),
        " Local Type Inference";

      "-specialize", Arg.Unit (append_xfrm Specialization.run_on_capsule),
        " Code Specialization";

      "-sccp", Arg.Unit (append_xfrm Constant_folding.run_on_capsule),
        " Sparse Conditional Code Propagation";
    ]) (fun arg ->
      inputs := arg :: !inputs)
    ("Usage: " ^ (Sys.argv.(0) ^ " [xfrmions] <input-file>..."));

  let input_ir =
    Unicode.Std.String.concat u""
      (List.map Io.input_all
        (List.map Io.open_in !inputs)) in

  let roots, capsule = load_ir (Lexing.from_string (input_ir :> string)) in

  List.iter (fun xfrm ->
      xfrm capsule)
    (List.rev !xfrms);

  let output_ir =
    if !no_roots then dump_ir capsule
    else dump_ir ~roots capsule
  in
  let out_chan  = Io.open_out !output in
    Unicode.Std.output_string out_chan output_ir
