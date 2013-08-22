open Unicode.Std
open Big_int
open Ssa

let run_on_function funcn =
  (* TODO don't allocate this each time :( *)
  let tvar () = Rt.Tvar (Rt.new_tvar ()) in
  let int_binop_ty =
    let tv = tvar () in Rt.FunctionTy ([tv; tv], tv) in
  let int_cmpop_ty =
    let tv = tvar () in Rt.FunctionTy ([tv; tv], Rt.BooleanTy) in

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
    -> (let _, return_ty = func_ty callee in
        if instr.ty <> return_ty then
          let env = Typing.unify instr.ty return_ty in
          Ssa.specialize funcn env)
    (* Primitives obey simple, primitive-specific typing rules. *)
    | PrimitiveInstr (prim, operands)
    -> (let env =
          match (prim :> latin1s) with
          (* Operands to integer primitives must have the
             same type. *)
          | "int_add"  | "int_sub"  | "int_mul"  | "int_and"
          | "int_or"   | "int_xor"  | "int_shl"  | "int_lshr"
          | "int_ashr" | "int_exp"
          -> (* Match de-facto type of this primitive invocation with
                its polymorphic signature. *)
             (let ty =
                match operands with
                | [a; b] -> Rt.FunctionTy ([a.Ssa.ty; b.Ssa.ty], instr.Ssa.ty)
                | _ -> assert false
              in
              let env = Typing.unify int_binop_ty ty in
              let ty  = Typing.subst env ty in
              (* Verify that the substitution yields a valid primitive
                 signature, i.e. it is indeed an integer operation. *)
              match ty with
              | Rt.FunctionTy(_, Rt.IntegerTy)
              | Rt.FunctionTy(_, Rt.UnsignedTy(_))
              | Rt.FunctionTy(_, Rt.SignedTy(_))
              -> Some env
              | _
              -> None)
          (* Tuple and record primitives have type-level semantics not
             expressible with (non-dependent) type variables. *)
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
