open Unicode.Std
open Ssa

let name = "Specialization"

let run_on_function passmgr capsule funcn =
  let overload orig_funcn ty =
    let orig_func  = func_of_name orig_funcn in
    let best_funcn =
      (* Try to find a more specific function, but not too specific. Unification
         of such function with the signature must produce the signature itself. *)
      try
        Ssa.find_overload capsule (Option.default orig_funcn orig_func.f_original)
          ~f:(fun overload_ty overload ->
            try
              let env = Typing.meaningful (Typing.unify overload_ty ty) in
              if Rt.equal ty (Typing.subst env ty) then begin
                (* Amount of the substitutions is used as a measurement of specifity. *)
                if Rt.equal overload_ty ty then
                  Some 99999 (* Exactly our type. *)
                else
                  Some (List.length env)
              end else
                None
            with Typing.Conflict _ ->
              None)
      with Not_found ->
        orig_funcn
    in
    (* Check if we need to specialize this function even more, or it's
       specific enough. *)
    let env = Typing.meaningful (Typing.unify best_funcn.ty ty) in
    if Rt.equal (Typing.subst env best_funcn.ty) best_funcn.ty then
      (* Unification produced a signature exactly equal to the overload
         being considered. *)
      best_funcn
    else begin
      (* Unification changed the signature of callee. *)
      let funcn' = copy_func best_funcn in
      add_func     capsule funcn';
      add_overload capsule (Option.default orig_funcn orig_func.f_original) ty funcn';
      ignore (specialize funcn' env);

      (* Visit newly added function. *)
      Pass_manager.mark ~reason:((Rt.inspect_type orig_funcn.ty) ^ " => " ^
                                 (Rt.inspect_type funcn'.ty))
                        passmgr funcn';

      funcn'
    end
  in

  iter_uses funcn ~f:(fun instr ->
    (* If the call site signature and callee types do not match,
       unify them and replace the callee with a specialized
       function. *)
    let specialize' callee sig_ty =
      if Rt.equal sig_ty callee.ty then
        callee
      else
        let callee' = overload callee sig_ty in
        if callee' != callee then begin
          (* Visit the caller again: specialization may have opened new
             opportunities. *)
          let caller = block_parent (instr_parent instr) in
          Pass_manager.mark ~reason:("specialized %" ^ instr.id)
                            passmgr caller
        end;
        callee'
    in
    let specialize callee sig_ty =
      try
        specialize' callee sig_ty
      with Typing.Conflict (a, b) ->
        IrPrinter.print_name instr;
        Rt.print_type a; Rt.print_type b;
        raise (Typing.Conflict (a, b))
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
        ty     = Rt.LambdaTy (arg_ty_elems, ret_ty); }
    -> (let arg_tys = Rt.tys_of_lambda_ty_elems arg_ty_elems in
        let sig_ty  = Rt.FunctionTy (frame.ty :: arg_tys, ret_ty) in
        let callee' = specialize callee sig_ty in
        set_opcode instr (ClosureInstr (callee', frame)))
    | _
    -> ())
