open Unicode.Std
open Big_int
open ExtList
open Ssa

let name = "Local Inference"

let stvar () = Rt.Tvar (Rt.new_static_tvar ())

let int_binop_ty = (* val int_binop : 'a -> 'a -> 'a *)
  let tv = stvar () in Rt.FunctionTy ([tv; tv], tv)
let int_cmpop_ty = (* val int_cmpop : 'a -> 'a -> bool *)
  let tv = stvar () in Rt.FunctionTy ([tv; tv], Rt.BooleanTy)

let run_on_function passmgr capsule funcn =
  let specialize ?reason env =
    if env <> [] && Ssa.specialize funcn env then
      Pass_manager.mark ?reason passmgr funcn
  in
  let local_var_ty frame name =
    let rec lookup ty =
      match Table.get ty.Rt.e_ty_bindings name with
      | Some binding -> binding.Rt.b_ty
      | None ->
        match ty.Rt.e_ty_parent with
        | Some ty -> lookup ty
        | None -> assert false
    in
    match frame.ty with
    | Rt.EnvironmentTy ty -> lookup ty
    | _ -> assert false
  in

  let ty_before_inference = funcn.ty in

  iter_instrs funcn ~f:(fun instr ->
    let unify ty ty' =
      try
        specialize ~reason:("unified " ^ (Rt.inspect_type ty) ^
                            " and " ^ (Rt.inspect_type ty') ^
                            " for " ^ instr.id)
                   (Typing.unify ty ty');
      with exn ->
        Pass_manager.print_exn exn ("inferring on %" ^ instr.id)
    in
    match instr.opcode with
    (* Make sure that call and return instruction types match the
       signatures of their associated functions. *)
    | ReturnInstr value
    -> (let _, return_ty = func_ty funcn in
        if not (Rt.equal value.ty return_ty) then
          unify return_ty value.ty)

    | CallInstr (callee, args)
    -> (match callee.ty with
        (* Call instruction does not necessarily point directly to
           the function; it can be a function pointer. *)
        | Rt.FunctionTy (args_ty', _)
        -> (* Unify call site signature with callee signature. *)
           (let args_ty = List.map (fun x -> x.ty) args in
            let sig_ty  = Rt.FunctionTy (args_ty, instr.ty) in
            unify callee.ty sig_ty)
        | _
        -> ())

    | ClosureInstr (callee, frame)
    -> (match callee.ty, instr.ty with
        | Rt.FunctionTy _, Rt.LambdaTy (arg_ty_elems, ret_ty)
        -> (let arg_tys = Rt.tys_of_lambda_ty_elems arg_ty_elems in
            let sig_ty = Rt.FunctionTy (frame.ty :: arg_tys, ret_ty) in
            unify callee.ty sig_ty)
        | _
        -> ())

    (* Local variable access. *)
    | LVarLoadInstr (frame, name)
    -> unify instr.ty (local_var_ty frame name)
    | LVarStoreInstr (frame, name, value)
    -> unify value.ty (local_var_ty frame name)

    (* Instance variable access. *)
    | IVarLoadInstr ({ ty = Rt.Class cls }, name)
    -> unify instr.ty (Typing.slot_ty cls name)
    | IVarStoreInstr ({ ty = Rt.Class cls }, name, value)
    -> unify value.ty (Typing.slot_ty cls name)

    (* Phi. *)
    | PhiInstr incoming
    -> (let tys = List.map (fun (_,value) -> value.ty) incoming in
        let env = Typing.unify_list (instr.ty :: tys) in
        specialize env)

    (* Tuples. *)
    | TupleExtendInstr ({ ty = Rt.TupleTy xs }, operands)
    -> (let operand_tys = List.map (fun x -> x.ty) operands in
        let ty = Rt.TupleTy (xs @ operand_tys) in
        unify instr.ty ty)
    | TupleConcatInstr ({ ty = Rt.TupleTy xs }, { ty = Rt.TupleTy ys })
    -> (let ty = Rt.TupleTy (xs @ ys) in
        unify instr.ty ty)

    (* Records. *)
    | RecordExtendInstr ({ ty = Rt.RecordTy xs }, operands)
    -> (try
          let operands = Assoc.sorted (List.map (fun (key, value) ->
              match key.opcode with
              | Const (Rt.Symbol name) -> name, value.ty
              | _ -> raise Exit)
            operands)
          in
          let ty = Rt.RecordTy operands in
          unify instr.ty ty
        with Exit ->
          ())
    | RecordConcatInstr ({ ty = Rt.RecordTy xs }, { ty = Rt.RecordTy ys })
    -> (let ty = Rt.RecordTy (Assoc.merge xs ys) in
        unify instr.ty ty)

    (* Primitives obey simple, primitive-specific typing rules. *)
    | PrimitiveInstr (prim, operands)
    -> (let unify_int_op int_op_ty =
          (* Match de-facto type of this primitive invocation with
              its polymorphic signature. *)
          let ty =
            match operands with
            | [a; b] -> Rt.FunctionTy ([a.Ssa.ty; b.Ssa.ty], instr.Ssa.ty)
            | _ -> assert false
          in
          let env = Typing.unify int_op_ty ty in
          let ty' = Typing.subst env ty in
          (* Verify that the substitution yields a valid primitive
             signature, i.e. it is indeed an integer operation. *)
          match ty' with
          | Rt.FunctionTy([Rt.IntegerTy;     Rt.IntegerTy],     _)
          | Rt.FunctionTy([Rt.UnsignedTy(_); Rt.UnsignedTy(_)], _)
          | Rt.FunctionTy([Rt.SignedTy(_);   Rt.SignedTy(_)],   _)
          -> (* Only specialize if the unification produced substitutions
                for the original function. As intop signatures are
                polymorphic, they will always produce non-empty env's. *)
             (if not (Rt.equal ty ty') then
                specialize env)
          | _
          -> ()
        in
        match (prim :> latin1s) with
        (* Debug primitive is polymorphic; it accepts any amount of
           values of any kind. *)
        | "debug"
        -> unify instr.ty Rt.NilTy

        (* Operands to integer primitives must have the
           same integral type. *)
        | "int_add"  | "int_sub"  | "int_mul" | "int_and"
        | "int_or"   | "int_xor"  | "int_shl" | "int_lshr"
        | "int_ashr" | "int_exp"
        -> unify_int_op int_binop_ty
        | "int_eq"   | "int_ne"   | "int_ule" | "int_sle"
        | "int_ult"  | "int_slt"  | "int_uge" | "int_sge"
        | "int_ugt"  | "int_sgt"
        -> unify_int_op int_cmpop_ty

        (* Tuple and record primitives have type-level semantics not
           expressible with type variables. *)
        | "tup_lookup"
        -> (match operands with
            | [ { ty     = Rt.TupleTy xs };
                { opcode = Const (Rt.Integer idx) } ]
            -> (let ty = List.nth xs (int_of_big_int idx) in
                unify instr.ty ty)
            | _
            -> ())
        | "tup_slice"
        -> (match operands with
            | [ { ty     = Rt.TupleTy xs };
                { opcode = Const (Rt.Integer lft) };
                { opcode = Const (Rt.Integer rgt) } ]
            -> (let lft = int_of_big_int lft
                and rgt = int_of_big_int rgt in
                let _, lft_slice = List.split_nth lft xs in
                let rgt_slice, _ = List.split_nth (rgt - lft) lft_slice in
                unify instr.ty (Rt.TupleTy rgt_slice))
            | _
            -> ())

        | "rec_lookup"
        -> (match operands with
            | [ { ty     = Rt.RecordTy xs };
                { opcode = Const (Rt.Symbol sym) } ]
            -> (let ty = Assoc.find xs sym in
                unify instr.ty ty)
            | _
            -> ())

        (* Object primitives. *)
        | "obj_alloc"
        -> (match operands with
            | [klass]
            -> (* The klass is not generally constant here; however, its type
                  is always known, and we can figure out the objectclass based
                  on metaclass. *)
               (match klass.ty with
                | Rt.Class ({ Rt.k_objectclass = None }, _)
                -> (* Someone is trying to allocate a value, or a Class instance. *)
                   (failwith "obj_alloc: objectclass=None")
                | Rt.Class ({ Rt.k_objectclass = Some klass }, specz')
                -> unify instr.ty (Rt.Class (klass, specz'))
                | _
                -> ())
            | _
            -> ())

        (* Memory access primitives. *)
        | "mem_store" | "mem_storev"
        -> unify instr.ty Rt.NilTy

        | _
        -> ())
    | _
    -> ());

  (* If the function never returns, change its signature to return nil. *)
  let does_return = ref false in
  iter_instrs funcn ~f:(fun instr ->
    match instr.opcode with
    | ReturnInstr _ -> does_return := true
    | _ -> ());

  if not !does_return then begin
    let args_ty, _ = func_ty funcn in
    set_ty funcn (Rt.FunctionTy (args_ty, Rt.NilTy))
  end;

  (* If function signature has changed, revisit all uses of this
     function. *)
  if not (Rt.equal funcn.ty ty_before_inference) then
    iter_uses funcn ~f:(fun user ->
      let funcn = block_parent (instr_parent user) in
      Pass_manager.mark ~reason:"signature changed" passmgr funcn);

  (* If operand types of any call instructions have changed, revisit
     the callee. Here, a weaker version of the check is implemented:
     if operand types are more specific than signature of any call
     instruction, revisit the callee.

     Note that this check is different from the one above: the former
     aims to substitute type variables in caller (this function) function
     with type information from callee, and the latter (the check below)
     aims to let the specialization pass decide if it wants to duplucate
     the body.
   *)
  iter_instrs funcn ~f:(fun instr ->
    match instr.opcode with
    | CallInstr (callee, operands)
    -> (match callee.opcode with
        | Function _
        -> (let args_ty = List.map (fun x -> x.ty) operands in
            let sig_ty  = Rt.FunctionTy (args_ty, instr.ty) in
            let env     = Typing.unify callee.ty sig_ty in
            let sig_ty' = Typing.subst env callee.ty in
            if not (Rt.equal sig_ty' callee.ty) then
              Pass_manager.mark ~reason:"signature mismatch" passmgr callee)
        | _
        -> ())
    | ClosureInstr (callee, frame)
    -> (match callee.opcode, instr.ty with
        | Function _, Rt.LambdaTy (arg_ty_elems, ret_ty)
        -> (let arg_tys = Rt.tys_of_lambda_ty_elems arg_ty_elems in
            let sig_ty  = Rt.FunctionTy (frame.ty :: arg_tys, ret_ty) in
            let env     = Typing.unify callee.ty sig_ty in
            let sig_ty' = Typing.subst env callee.ty in
            if not (Rt.equal sig_ty' callee.ty) then
              Pass_manager.mark ~reason:"signature mismatch" passmgr callee)
        | _
        -> ())
    | _
    -> ())
