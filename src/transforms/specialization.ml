open Unicode.Std
open Ssa

let name = "Specialization"

let overload capsule orig_funcn ty =
  let best_funcn =
    (* Try to find a more specific function, but not too specific. Unification
       of such function with the signature must produce the signature itself. *)
    try
      Ssa.find_overload capsule orig_funcn ~f:(fun overload ->
        try
          let env = Typing.meaningful (Typing.unify overload.ty ty) in
          if Rt.equal ty (Typing.subst env ty) then begin
            (* Amount of the substitutions is used as a measurement of specifity. *)
            if Rt.equal overload.ty ty then
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
      add_func capsule funcn';
      add_overload capsule best_funcn funcn';
      ignore (specialize funcn' env);
      funcn'
    end

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
