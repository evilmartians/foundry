open Unicode.Std
open Ssa

let should_inline funcn =
  false

let run_on_function funcn =
  (* Find all call instructions eligible for inlining. *)
  iter_instrs funcn ~f:(fun instr ->
    match instr.opcode with
    | CallInstr (callee, operands)
    -> (if should_inline callee then
          ())
    | _
    -> ())

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
