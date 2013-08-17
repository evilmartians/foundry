let load_ir lexbuf =
  let lex () = IrLexer.next lexbuf in
  let parse  = MenhirLib.Convert.Simplified.traditional2revised IrParser.toplevel in
  parse lex

let dump_ir roots capsule =
  IrPrinter.print roots capsule

let _ =
  let output = ref "-" in
  let inputs = ref []  in
  let optzns = ref []  in

  Arg.parse (Arg.align [
      "-o", Arg.Set_string output,
        "<file> Output file";

      "-ordered", Arg.Set IrPrinter.ordered,
        " Iterate symbol tables in alphabetical order";

      "-dce", Arg.Unit (fun () ->
          optzns := Dead_code_elim.run_on_capsule :: !optzns),
        "Dead Code Elimination"
    ]) (fun arg ->
      inputs := arg :: !inputs)
    ("Usage: " ^ (Sys.argv.(0) ^ " [options] <input-file>..."));

  let input_ir =
    Unicode.Std.String.concat u""
      (List.map Io.input_all
        (List.map Io.open_in !inputs)) in

  let roots, capsule = load_ir (Lexing.from_string (input_ir :> string)) in

  List.iter (fun optzn ->
      optzn capsule)
    !optzns;

  let out_chan = Io.open_out !output in
    Unicode.Std.output_string out_chan (dump_ir roots capsule)
