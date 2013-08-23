open Unicode.Std
open Big_int
open Ssa

let stvar () = Rt.Tvar (Rt.new_static_tvar ())

let int_binop_ty = (* val int_binop : 'a -> 'a -> 'a *)
  let tv = stvar () in Rt.FunctionTy ([tv; tv], tv)
let int_cmpop_ty = (* val int_cmpop : 'a -> 'a -> bool *)
  let tv = stvar () in Rt.FunctionTy ([tv; tv], Rt.BooleanTy)

let run_on_function funcn =
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
  iter_instrs funcn ~f:(fun instr ->
    match instr.opcode with
    (* Make sure that call and return instruction types match the
       signatures of their associated functions. *)
    | ReturnInstr value
    -> (let _, return_ty = func_ty funcn in
        if value.ty <> return_ty then
          let env = Typing.unify return_ty value.ty in
          Ssa.specialize funcn env)
    | CallInstr (callee, _)
    -> (* Call instruction does not necessarily point directly to
          the function; it can be a function pointer. *)
       (match callee.ty with
        | Rt.FunctionTy (_, return_ty)
        -> (if instr.ty <> return_ty then
              let env = Typing.unify instr.ty return_ty in
              Ssa.specialize funcn env)
        | _
        -> ())
    (* Local variable accesses are unification points. *)
    | LVarLoadInstr (frame, name)
    -> (let env = Typing.unify instr.ty (local_var_ty frame name) in
        Ssa.specialize funcn env)
    | LVarStoreInstr (frame, name, value)
    -> (let env = Typing.unify value.ty (local_var_ty frame name) in
        Ssa.specialize funcn env)
    (* Phis are unification points. *)
    | PhiInstr incoming
    -> (let tys = List.map (fun (_,value) -> value.ty) incoming in
        let env = Typing.unify_list (instr.ty :: tys) in
        Ssa.specialize funcn env)
    (* Primitives obey simple, primitive-specific typing rules. *)
    | PrimitiveInstr (prim, operands)
    -> (let env =
          let unify_int_op int_op_ty =
            (* Match de-facto type of this primitive invocation with
                its polymorphic signature. *)
            let ty =
              match operands with
              | [a; b] -> Rt.FunctionTy ([a.Ssa.ty; b.Ssa.ty], instr.Ssa.ty)
              | _ -> assert false
            in
            let env = Typing.unify int_op_ty ty in
            let ty  = Typing.subst env ty in
            (* Verify that the substitution yields a valid primitive
               signature, i.e. it is indeed an integer operation. *)
            match ty with
            | Rt.FunctionTy([Rt.IntegerTy;     Rt.IntegerTy],     _)
            | Rt.FunctionTy([Rt.UnsignedTy(_); Rt.UnsignedTy(_)], _)
            | Rt.FunctionTy([Rt.SignedTy(_);   Rt.SignedTy(_)],   _)
            -> Some env
            | _
            -> None
          in
          match (prim :> latin1s) with
          (* Debug primitive is polymorphic; it accepts any amount of
             values of any kind. *)
          | "debug"
          -> (let env = Typing.unify instr.ty Rt.NilTy in
              Some env)
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
              let env = Typing.unify instr.ty ty in
              Some env)
          | "tup_extend"
          -> (match operands with
              | [ { ty = Rt.TupleTy xs };
                  { ty = x } ]
              -> (let ty = Rt.TupleTy (xs @ [x]) in
                  Some (Typing.unify instr.Ssa.ty ty))
              | _
              -> None)
          | "tup_index"
          -> (match operands with
              | [ { ty = Rt.TupleTy xs };
                  { opcode = Const (Rt.Integer idx) } ]
              -> (let ty = List.nth xs (int_of_big_int idx) in
                  Some (Typing.unify instr.Ssa.ty ty))
              | _
              -> None)
          | _
          -> None
        in Option.may (specialize funcn) env)
    | _
    -> ())

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
