let _ =
  let output        = ref "-"
  and no_roots      = ref false
  and inputs        = ref []
  and passmgr_stack = ref []
  and passmgr       = ref (Pass_manager.create ~sequential:true)
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
          passmgr := Toolchain.build_pass_manager ()),
        " Include standard transformations";

      "-worklist", Arg.Unit (fun () ->
          passmgr := Pass_manager.create ~sequential:false),
        " Erase all pending passes, and replace pass manager with a worklist-based one";

      "-[", Arg.Unit (fun () ->
          passmgr_stack := !passmgr :: !passmgr_stack;
          passmgr := Pass_manager.create ~sequential:true),
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

      "-coerce", Arg.Unit (fun () ->
          Pass_manager.add_function_pass !passmgr (module Coercion)),
        " Literal Coercion";
    ]) (fun arg ->
      inputs := arg :: !inputs)
    ("Usage: " ^ Sys.argv.(0) ^ " [options] <input-file>...");

  if !inputs = [] then
    inputs := ["-"];

  let input_ir = Unicode.Std.String.concat u""
                    (List.map Io.input_file !inputs) in

  let roots, capsule = Toolchain.parse_ir (Lexing.from_string (input_ir :> string)) in
  Rt.roots := roots;

  Pass_manager.run !passmgr capsule;

  let output_ir = Toolchain.print_ir ~omit_roots:!no_roots (roots, capsule) in
  Io.output_file !output output_ir
