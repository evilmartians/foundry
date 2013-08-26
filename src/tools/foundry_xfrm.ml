let load_ir lexbuf =
  let lex () = IrLexer.next lexbuf in
  let parse  = MenhirLib.Convert.Simplified.traditional2revised IrParser.toplevel in
  try
    parse lex
  with IrParser.StateError _ ->
    let pos = Lexing.lexeme_start_p lexbuf in
    prerr_endline ("Invalid input near " ^ (string_of_int pos.Lexing.pos_lnum) ^ ":" ^
                   (string_of_int pos.Lexing.pos_bol));
    exit 1

let dump_ir omit_roots roots capsule =
  IrPrinter.string_of ~omit_roots roots capsule

let _ =
  let output        = ref "-"
  and no_roots      = ref false
  and inputs        = ref []
  and passmgr_stack = ref []
  and passmgr       = ref (Pass_manager.create ~sequental:true)
  in

  Arg.parse (Arg.align [
      "-o", Arg.Set_string output,
        "<file> Output file";

      "-ordered", Arg.Set IrPrinter.ordered,
        " Iterate symbol tables in alphabetical order";

      "-no-roots", Arg.Set no_roots,
        " Don't print out root data structures";

      "-verbose", Arg.Set Pass_manager.verbose,
        " Print each transformation pass and invalidation as they're performed";

      "-std-xfrms", Arg.Unit (fun () ->
          let dep_passmgr = Pass_manager.create ~sequental:false in
            Pass_manager.add_function_pass dep_passmgr (module Local_inference);
            Pass_manager.add_function_pass dep_passmgr (module Method_resolution);
            Pass_manager.add_function_pass dep_passmgr (module Specialization);
            Pass_manager.add_function_pass dep_passmgr (module Local_inference);
            Pass_manager.add_function_pass dep_passmgr (module Constant_folding);
          Pass_manager.add_pass_manager !passmgr dep_passmgr;
          Pass_manager.add_capsule_pass !passmgr (module Global_dce)),
        " Include standard transformations";

      "-worklist", Arg.Unit (fun () ->
          passmgr := Pass_manager.create ~sequental:false),
        " Erase all pending passes, and replace pass manager with a worklist-based one";

      "-[", Arg.Unit (fun () ->
          passmgr_stack := !passmgr :: !passmgr_stack;
          passmgr := Pass_manager.create ~sequental:true),
        " Push a pass manager";

      "-]", Arg.Unit (fun () ->
          let outer_passmgr = List.hd !passmgr_stack in
          Pass_manager.add_pass_manager outer_passmgr !passmgr;
          passmgr_stack := List.tl !passmgr_stack;
          passmgr       := outer_passmgr),
        " Pop a pass manager and merge current pass manager into it";

      "-dce", Arg.Unit (fun () ->
          Pass_manager.add_function_pass !passmgr (module Dead_code_elim)),
        " Dead Code Elimination";

      "-gdce", Arg.Unit (fun () ->
          Pass_manager.add_capsule_pass !passmgr (module Global_dce)),
        " Global Dead Code Elimination";

      "-simplify-cfg", Arg.Unit (fun () ->
          Pass_manager.add_function_pass !passmgr (module Cfg_simplification)),
        " CFG Simplification";

      "-simplify-frames", Arg.Unit (fun () ->
          Pass_manager.add_function_pass !passmgr (module Frame_simplification)),
        " Frame Simplification";

      "-infer", Arg.Unit (fun () ->
          Pass_manager.add_function_pass !passmgr (module Local_inference)),
        " Local Type Inference";

      "-specialize", Arg.Unit (fun () ->
          Pass_manager.add_function_pass !passmgr (module Specialization)),
        " Code Specialization";

      "-sccp", Arg.Unit (fun () ->
          Pass_manager.add_function_pass !passmgr (module Constant_folding)),
        " Sparse Conditional Code Propagation";

      "-resolve", Arg.Unit (fun () ->
          Pass_manager.add_function_pass !passmgr (module Method_resolution)),
        " Method Resolution";
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

  Pass_manager.run !passmgr capsule;

  let output_ir = dump_ir !no_roots roots capsule in
  let out_chan  = Io.open_out !output in
    Unicode.Std.output_string out_chan output_ir
