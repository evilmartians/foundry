open Unicode.Std
open Big_int
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
      match Table.get ty.Rt.e_bindings_ty name with
      | Some binding -> binding.Rt.b_value_ty
      | None ->
        match ty.Rt.e_parent_ty with
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
      specialize ~reason:("unified " ^ (Rt.inspect_type ty) ^
                          " and " ^ (Rt.inspect_type ty') ^
                          " for " ^ instr.id)
                 (Typing.unify ty ty');
    in
    match instr.opcode with
    (* Make sure that call and return instruction types match the
       signatures of their associated functions. *)
    | ReturnInstr value
    -> (let _, return_ty = func_ty funcn in
        if not (Rt.equal value.ty return_ty) then
          unify return_ty value.ty)
    | CallInstr (callee, _)
    -> (* Call instruction does not necessarily point directly to
          the function; it can be a function pointer. *)
       (match callee.ty with
        | Rt.FunctionTy (_, return_ty)
        -> (if not (Rt.equal instr.ty return_ty) then
              unify instr.ty return_ty)
        | _
        -> ())
    (* Local variable accesses are unification points. *)
    | LVarLoadInstr (frame, name)
    -> unify instr.ty (local_var_ty frame name)
    | LVarStoreInstr (frame, name, value)
    -> unify value.ty (local_var_ty frame name)
    (* Phis are unification points. *)
    | PhiInstr incoming
    -> (let tys = List.map (fun (_,value) -> value.ty) incoming in
        let env = Typing.unify_list (instr.ty :: tys) in
        specialize env)
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
        | "debug" | "putchar"
        -> unify instr.ty Rt.NilTy
        (* Operands to integer primitives must have the
           same integral type. *)
        | "int_add"  | "int_sub"  | "int_mul" | "int_and"
        | "int_or"   | "int_xor"  | "int_shl" | "int_lshr"
        | "int_ashr" | "int_exp"
        -> unify_int_op int_binop_ty
        | "int_eq"   | "int_neq"  | "int_ule" | "int_sle"
        | "int_ult"  | "int_slt"  | "int_uge" | "int_sge"
        | "int_ugt"  | "int_sgt"
        -> unify_int_op int_cmpop_ty
        (* Tuple and record primitives have type-level semantics not
           expressible with (non-dependent) type variables. *)
        | "tup_make"
        -> (let ty  = Rt.TupleTy (List.map (fun x -> x.ty) operands) in
            unify instr.ty ty)
        | "tup_extend"
        -> (match operands with
            | [ { ty = Rt.TupleTy xs };
                { ty = x } ]
            -> (let ty = Rt.TupleTy (xs @ [x]) in
                unify instr.ty ty)
            | _
            -> ())
        | "tup_index"
        -> (match operands with
            | [ { ty = Rt.TupleTy xs };
                { opcode = Const (Rt.Integer idx) } ]
            -> (let ty = List.nth xs (int_of_big_int idx) in
                unify instr.ty ty)
            | _
            -> ())
        | _
        -> ())
    | _
    -> ());

  (* If function signature has changed, revisit all uses of this
     function. *)
  if not (Rt.equal funcn.ty ty_before_inference) then
    (* Revisit all uses of this function. *)
    iter_uses funcn ~f:(fun user ->
      let funcn = block_parent (instr_parent user) in
      Pass_manager.mark ~reason:"signature changed" passmgr funcn);

  (* If operand types of any call instructions have changed, revisit
     the callee. Here, a weaker version of the check is implemented:
     if operand types are more specific than signature of any call
     instruction, revisit the callee. *)
  iter_instrs funcn ~f:(fun instr ->
    match instr.opcode with
    | CallInstr (callee, operands)
    -> (match callee with
        | { opcode = Function _; ty = callee_sig ;}
        -> (let operands_ty = List.map (fun x -> x.ty) operands in
            let caller_sig  = Rt.FunctionTy (operands_ty, instr.ty) in
            let env         = Typing.unify callee_sig caller_sig in
            let unified_sig = Typing.subst env callee_sig in
            if not (Rt.equal unified_sig callee_sig) then
              Pass_manager.mark ~reason:"signature mismatch" passmgr callee)
        | _
        -> ())
    | _
    -> ())
