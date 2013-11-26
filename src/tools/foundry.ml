let error message =
  prerr_endline ("foundry: error: " ^ message)

let error_and_exit message =
  error message;
  exit 1

let lltargets =
  Llvm_X86.initialize ();
  Llvm_ARM.initialize ();
  Llvm_target.Target.all ()

let _ =
  let arch      = ref ""
  and triple    = ref ""
  and cpu       = ref ""
  and features  = ref ""
  and emit_ir   = ref false
  and emit_llvm = ref false
  and emit_text = ref false
  and output    = ref ""
  and inputs    = ref []
  in

  Arg.parse (Arg.align [
      "-arch", Arg.Set_string arch,
        "<arch> Target architecture";

      "-triple", Arg.Set_string triple,
        "<triple> Target triple";

      "-cpu", Arg.Set_string cpu,
        "<cpu> Target CPU";

      "-attr", Arg.Set_string features,
        "<features> Target CPU features";

      "-S", Arg.Set emit_text,
        " Emit assembly instead of object code or bitcode";

      "-E", Arg.Set emit_ir,
        " Emit Foundry IR";

      "-emit-llvm", Arg.Set emit_llvm,
        " Emit LLVM IR";

      "-o", Arg.Set_string output,
        "<file> Output file";

      "-version", Arg.Unit (fun () ->
          prerr_endline "Foundry alpha";
          prerr_endline ("Targets: " ^
              (String.concat ", "
                  (List.map Llvm_target.Target.name lltargets)));
          exit 0),
        " Show version and configuration information";
    ]) (fun arg ->
      inputs := arg :: !inputs)
    ("Usage: " ^ Sys.argv.(0) ^ " <input-file>...");

  if !emit_ir && !emit_llvm then
    error_and_exit "-E is incompatible with -emit-llvm";

  if !inputs = [] then
    error_and_exit "no input files";

  if !output = "" then begin
    let suffix =
      if !emit_ir then
        ".fir"
      else if !emit_llvm && !emit_text then
        ".ll"
      else if !emit_llvm && (not !emit_text) then
        ".bc"
      else if !emit_text then
        ".s"
      else
        ".o"
    in
    let prefix =
      match !inputs with
      | "-"      :: _ -> "foundry"
      | filename :: _ -> filename
      | _ -> assert false
    in
    output := prefix ^ suffix
  end;

  if !arch = "" then
    error_and_exit "specify architecture with -arch";

  let lltarget =
    try
      List.find (fun lltarget ->
          (Llvm_target.Target.name lltarget) = !arch)
        lltargets
    with Not_found ->
      error_and_exit ("unknown architecture " ^ !arch)
  in
  let llmachine =
    Llvm_target.TargetMachine.create ~triple:!triple ~cpu:!cpu ~features:!features
        lltarget
  in
  let lllayout    = Llvm_target.TargetMachine.data_layout llmachine in
  let lllayoutstr = Llvm_target.DataLayout.as_string lllayout in

  let env = Vm.env_create () in
  List.iter (fun filename ->
      let source = Toolchain.input_file filename in
      match Toolchain.parse_eval_source env source with
      | Toolchain.Success value
      -> ()
      | Toolchain.Failure diags
      -> (List.iter Diagnostic.print diags;
          exit 1))
    (List.rev !inputs);

  let passmgr = Toolchain.build_pass_manager ()
  and capsule = Toolchain.bootstrap_capsule !Rt.roots in
  Pass_manager.run passmgr capsule;

  if !emit_ir then begin
    let output_ir = Toolchain.print_ir (!Rt.roots, capsule) in
    Io.output_file !output output_ir;
    exit 0
  end;

  let llmod = Llvm_gen.llvm_module_of_ssa_capsule capsule in
  Llvm.set_target_triple !triple llmod;
  Llvm.set_data_layout lllayoutstr llmod;

  let llpassmgr = Llvm.PassManager.create () in
  Llvm_target.DataLayout.add_to_pass_manager llpassmgr lllayout;

  let llpmbuilder = Llvm_passmgr_builder.create () in
  Llvm_passmgr_builder.set_opt_level 2 llpmbuilder;
  Llvm_passmgr_builder.set_size_level 1 llpmbuilder;
  Llvm_passmgr_builder.use_inliner_with_threshold 275 llpmbuilder;
  Llvm_passmgr_builder.populate_module_pass_manager llpassmgr llpmbuilder;
  Llvm_passmgr_builder.populate_lto_pass_manager
      ~internalize:true ~run_inliner:true llpassmgr llpmbuilder;
  ignore (Llvm.PassManager.run_module llmod llpassmgr);

  if !emit_llvm then begin
    if !emit_text then begin
      if !output = "-" then begin
        let tmpfile = Filename.temp_file "llvmasm" ".ll" in
        Llvm.print_module tmpfile llmod;
        let llasm = Io.input_file tmpfile in
        Sys.remove tmpfile;
        print_endline (llasm :> string)
      end else
        Llvm.print_module !output llmod
    end else begin
      let chan = Io.open_out !output in
      if not (Llvm_bitwriter.output_bitcode chan llmod) then
        error_and_exit ("cannot write bitcode to " ^ !output);
      close_out chan
    end;
    exit 0
  end;

  let llfiletype =
    if !emit_text then
      Llvm_target.CodeGenFileType.AssemblyFile
    else
      Llvm_target.CodeGenFileType.ObjectFile
  in
  let llbuf = Llvm_target.TargetMachine.emit_to_memory_buffer llmod llfiletype llmachine in
  let chan  = Io.open_out !output in
  output_string chan (Llvm.MemoryBuffer.as_string llbuf)
