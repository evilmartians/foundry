open Unicode.Std
open Ssa

let name = "Specialization"

let run_on_function passmgr capsule funcn =
  iter_uses funcn ~f:(fun instr ->
    (* If the call site signature and callee types do not match,
       unify them and replace the callee with a specialized
       function. *)
    let specialize callee sig_ty =
      if Rt.equal sig_ty callee.ty then
        callee
      else
        let callee' = overload capsule callee sig_ty in
        if callee' != callee then begin
          (* Visit newly added function. *)
          Pass_manager.mark ~reason:((Rt.inspect_type callee.ty) ^ " => " ^
                                     (Rt.inspect_type sig_ty))
                            passmgr callee';
        end;
        callee'
    in
    match instr with
    (* Find all call instructions and consider specializing
       them if the callee is constant. *)
    | { opcode = CallInstr ({ opcode = Function _ } as callee, args) }
    -> (let args_ty = List.map (fun x -> x.ty) args in
        let sig_ty  = Rt.FunctionTy (args_ty, instr.ty) in
        let callee' = specialize callee sig_ty in
        set_opcode instr (CallInstr (callee', args)))
    (* Do the same for closures. *)
    | { opcode = ClosureInstr ({ opcode = Function _ } as callee, frame);
        ty     = Rt.ClosureTy (args_ty, ret_ty); }
    -> (let sig_ty  = Rt.FunctionTy (frame.ty :: args_ty, ret_ty) in
        let callee' = specialize callee sig_ty in
        set_opcode instr (ClosureInstr (callee', frame)))
    | _
    -> ())
