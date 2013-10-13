open Fy_big_int

let _ =
  let output   = ref "-" in
  let inputs   = ref []  in

  Arg.parse (Arg.align [
      "-o", Arg.Set_string output,
        "<file> Output file";
    ]) (fun arg ->
      inputs := arg :: !inputs)
    ("Usage: " ^ Sys.argv.(0) ^ " <input-file>...");

  if !inputs = [] then
    inputs := ["-"];

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

  let capsule   = Toolchain.bootstrap_capsule !Rt.roots in
  let output_ir = Toolchain.print_ir (!Rt.roots, capsule) in
  Io.output_file !output output_ir
