open Unicode.Std
open Ssa

let run_on_capsule capsule =
  let maybe_specialize caller callee operands =
    let operands_ty = List.map (fun x -> x.ty) operands in
    match callee.ty with
    | Rt.FunctionTy (args_ty, return_ty)
    -> (* If the argument types and operand types differ, hypothesize that
          operand types are more specific, and specialize the callee for
          operand types. *)
       (if args_ty <> operands_ty then
          Some (overload capsule callee operands_ty)
        else
          None)
    | _
    -> assert false
  in
  iter_funcs capsule ~f:(fun caller ->
    iter_instrs caller ~f:(fun instr ->
      (* Find all call instructions and consider specializing
         them if the callee is constant. *)
      match instr.opcode with
      | CallInstr ({ opcode = Function _ } as callee, operands)
      -> (match maybe_specialize caller callee operands with
          | Some callee'
          -> set_opcode (CallInstr (callee', operands)) instr
          | None
          -> ())
      | _
      -> ()))
