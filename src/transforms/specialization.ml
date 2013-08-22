open Unicode.Std
open Ssa

let run_on_capsule capsule =
  iter_funcs capsule ~f:(fun caller ->
    iter_instrs caller ~f:(fun instr ->
      (* Find all call instructions and consider specializing
         them if the callee is constant. *)
      match instr.opcode with
      | CallInstr ({ opcode = Function _ } as callee, operands)
      -> (let operands_ty = List.map (fun x -> x.ty) operands in
          let call_site_ty = Rt.FunctionTy (operands_ty, instr.ty) in
          (* If the call site signature and callee types do not match,
             unify them and replace the callee with a specialized
             function. *)
          if call_site_ty <> callee.ty then
            let callee' = overload capsule callee call_site_ty in
            set_opcode instr (CallInstr (callee', operands)))
      | _
      -> ()))
