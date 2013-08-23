open Fy_big_int

let parse filename =
  let content  = Io.input_all (Io.open_in filename) in
  let file     = Location.register (Unicode.assert_utf8s filename) 1 content in
  let lexstate = Lexer.create file in
  let lexbuf   = Ulexing.from_utf8_string (content :> string) in
  let lex ()   = Lexer.next lexstate lexbuf in
  let parse    = MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel in
  parse lex

let _ =
  let output   = ref "-"   in
  let inputs   = ref []    in

  Arg.parse (Arg.align [
      "-o", Arg.Set_string output,
        "<file> Output file";
    ]) (fun arg ->
      inputs := arg :: !inputs)
    ("Usage: " ^ Sys.argv.(0) ^ " <input-file>...");

  if !inputs = [] then
    inputs := ["-"];

  let env = Vm.env_create () in

  List.iter (fun input ->
      try
        let ast = parse input in
        match Verifier.check ast with
        | []
        -> ignore (Vm.eval env (parse input))
        | diags
        -> (List.iter Diagnostic.print diags;
            exit 1)
      with
      | Parser.StateError (token, state)
      -> (let diag = Diagnostic.Error,
                     Unicode.assert_utf8s (Parser_errors.message state token),
                     [Parser_desc.loc_of_token token] in
          Diagnostic.print diag;
          exit 1)
      | Rt.Exc exc
      -> (let diag = Diagnostic.Error, exc.Rt.ex_message, exc.Rt.ex_locations in
          Diagnostic.print diag;
          exit 1))
    !inputs;

  let capsule = Ssa.create_capsule () in
    let funcn    = Ssa.create_func ~id:u"main" [] (Rt.UnsignedTy 32) in
    Ssa.add_func capsule funcn;

    let entry    = Ssa.create_block ~id:u"entry" funcn in
    let toplevel = Ssa.name_of_value (Rt.Package (!Rt.roots).Rt.pToplevel) in

    let resolve  = Ssa.create_instr (Rt.Tvar (Rt.new_tvar ()))
                      (Ssa.ResolveInstr (toplevel,
                          Ssa.name_of_value (Rt.Symbol u"main"))) in
    Ssa.append_instr resolve entry;

    let call     = Ssa.create_instr (Rt.Tvar (Rt.new_tvar ()))
                      (Ssa.CallInstr (resolve, [
                          Ssa.name_of_value (Rt.Tuple ([
                              Rt.Package (!Rt.roots).Rt.pToplevel ]));
                          Ssa.name_of_value (Rt.Record (
                              Table.create []))
                       ])) in
    Ssa.append_instr call entry;

    let return   = Ssa.create_instr (Rt.UnsignedTy 32)
                      (Ssa.ReturnInstr (
                          Ssa.name_of_value (Rt.Unsigned (32, (big_int_of_int 0))))) in
    Ssa.append_instr return entry;

  let output_ir = IrPrinter.string_of !Rt.roots capsule in
  let out_chan  = Io.open_out !output in
    Unicode.Std.output_string out_chan output_ir
