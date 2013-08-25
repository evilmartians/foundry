open Unicode.Std
open Ssa

let name = "Specialization"

let overload capsule funcn ty' =
  let funcn =
    (* Try to find a more specific function, but not too specific. Unification
       of such function with the signature must produce the signature itself. *)
    try
      Ssa.find_overload capsule funcn ~f:(fun overload ->
        try
          let env = Typing.unify ty' overload.ty in
          (* Discard all tvar->tvar association from the type substitutions.
             Unification succeeded, which means the signatures are compatible;
             type variable sets are considered disjoint in different functions,
             so it does not matter which ones associate to which. *)
          let env = List.filter (fun (tv,ty) ->
                        match ty with
                        | Rt.Tvar _ -> false
                        | _ -> true)
                      env in
          Rt.equal ty' (Typing.subst env ty')
        with Typing.Conflict _ -> false)
    with Not_found ->
      funcn
  in
    (* Check if we need to specialize this function even more, or it's
       specific enough. *)
    let env = Typing.unify funcn.ty ty' in
    if Rt.equal (Typing.subst env funcn.ty) funcn.ty then
      (* Unification produced a signature exactly equal to the overload
         being considered. *)
      funcn
    else begin
      (* Unification changed the signature of callee. *)
      let funcn' = copy_func funcn in
      add_func capsule funcn';
      add_overload capsule funcn funcn';
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
