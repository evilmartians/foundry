open Unicode.Std
open Ssa

let run_on_function funcn =
  (* For every terminator... *)
  iter_blocks funcn ~f:(fun blockn ->
    match (terminator blockn).opcode with
    | ReturnInstr value
    -> (match funcn.ty with
        | Rt.FunctionTy (_, return_ty)
        -> (* Find all return instructions whose type differs
              from the return type of this function. With a hypothesis
              that the instruction type is more specific, narrow
              the function type. *)
           (if value.ty <> return_ty then
              let subst = Typing.match_ty return_ty value.ty in
              Ssa.specialize funcn subst)
        | _
        -> assert false)
    | _
    -> ());

  (* For every instruction... *)
  iter_instrs funcn ~f:(fun instr ->
    match instr.opcode with
    | CallInstr (callee, _)
    -> (match callee.ty with
        | Rt.FunctionTy (_, return_ty)
        -> (* Find all call instructions whose type differs from the
              return type of the callee. With a hypothesis that the
              function type is more specific, narrow the instruction
              type. (Reverse of the above.) *)
           (if instr.ty <> return_ty then
              let subst = Typing.match_ty instr.ty return_ty in
              Ssa.specialize funcn subst)
        | _
        -> assert false)
    | _
    -> ())

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
