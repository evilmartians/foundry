open Unicode.Std
open Ssa
open ExtList
open Big_int

let name = "Method Resolution"

let parse_arguments ~before ~arg_tys ~args ~kwargs =
  (* Internally args and kwargs are treated disjointly. Split syntactic
     args to better match this representation. *)
  let ty_arg_elems, ty_kwarg_elems =
    List.partition (fun arg ->
        match arg with
        | Rt.LambdaArg _   | Rt.LambdaOptArg _   | Rt.LambdaRest _
        -> true
        | Rt.LambdaKwArg _ | Rt.LambdaKwOptArg _ | Rt.LambdaKwRest _
        -> false)
      arg_tys
  in
  (* Find out how args to skip from the end for *rest. *)
  let rest_index =
    try
      fst (List.findi (fun _ arg ->
              match arg with
              | Rt.LambdaRest _ -> true
              | _ -> false)
            ty_arg_elems)
    with Not_found ->
      (* The value of rest_index is never used if there is no
         FormalRest in the args. 0 works. *)
      0
  in
  let post_count = (List.length ty_arg_elems) - rest_index - 1 in
  (* Make sure the order of evaluation matches lexical order of
     arguments. *)
  let arg_idx = ref 0 in
  let append ?(ty=Rt.tvar_as_ty ()) instr =
    let instr = Ssa.create_instr ty instr in
    Ssa.prepend_instr ~before instr (instr_parent before);
    instr
  in
  List.map (fun ty_elem ->
      (* Helper functions to assemble type-level calculations. *)
      let int n = Ssa.const (Rt.Integer (big_int_of_int n))
      and sym s = Ssa.const (Rt.Symbol s) in
      let right_idx idx =
        let len = append (Ssa.PrimitiveInstr ("tup_length", [args])) in
        append (Ssa.PrimitiveInstr ("int_sub", [len; idx]))
      in
      match ty_elem with
      | Rt.LambdaArg ty
      -> (let idx =
            if !arg_idx >= 0 then begin
              let idx = int !arg_idx in
              incr arg_idx;
              idx
            end else begin
              let idx = right_idx (int (-(!arg_idx))) in
              decr arg_idx;
              idx
            end
          in
          append ~ty (Ssa.PrimitiveInstr ("tup_lookup", [args; idx])))
      | Rt.LambdaOptArg ty
      -> (let idx  = int !arg_idx in
          incr arg_idx;
          let tup_len = append (Ssa.PrimitiveInstr ("tup_length",  [args])) in
          let has_arg = append (Ssa.PrimitiveInstr ("int_gt",      [tup_len; idx]))
          and arg     = append (Ssa.PrimitiveInstr ("tup_lookup",  [args; idx])) in
          let full    = append (Ssa.PrimitiveInstr ("opt_alloc",   [arg]))
          and empty   = Ssa.const (Rt.Option (Rt.Empty ty)) in
          append ~ty:(Rt.OptionTy ty) (Ssa.SelectInstr (has_arg, full, empty)))
      | Rt.LambdaRest ty
      -> (let last_idx = right_idx (int post_count) in
          append ~ty (Ssa.PrimitiveInstr ("tup_slice", [args; int !arg_idx; last_idx])))
      | Rt.LambdaKwArg (kw, ty)
      -> append ~ty (Ssa.PrimitiveInstr ("rec_lookup", [kwargs; sym kw]))
      | Rt.LambdaKwOptArg (kw, ty)
      -> (let has_arg = append (Ssa.PrimitiveInstr ("rec_incl",    [kwargs; sym kw]))
          and arg     = append (Ssa.PrimitiveInstr ("rec_lookup",  [kwargs; sym kw])) in
          let full    = append (Ssa.PrimitiveInstr ("opt_alloc",   [arg]))
          and empty   = Ssa.const (Rt.Option (Rt.Empty ty)) in
          append ~ty:(Rt.OptionTy ty) (Ssa.SelectInstr (has_arg, full, empty)))
      | Rt.LambdaKwRest ty
      -> kwargs (* TODO stub *))
    arg_tys

let run_on_function passmgr capsule caller =
  iter_instrs caller ~f:(fun instr ->
    (* Find all send instructions where:
       - Selector is fully known;
       - Type of receiver is at least partially known, i.e.
         not a bare type variable. Method lookup in Foundry is
         only dependent on the type itself and not its parameters. *)
    match instr.opcode with
    | PrimitiveInstr (prim, [recvn; { opcode = Const (Rt.Symbol selector) };
                             args; kwargs]) when prim = "obj_send"
    -> (match recvn with
        | { ty = Rt.Tvar _ }
        -> ()

        | { ty }
        -> (let rec lookup klass selector =
              try
                klass, Assoc.find klass.Rt.k_methods selector
              with Not_found ->
                match klass.Rt.k_ancestor with
                | Some ancestor -> lookup ancestor selector
                | None -> raise Not_found
            in
            let klass = Rt.klass_of_type ty in
            let klass, imethod =
              try lookup klass selector
              with Not_found ->
                try lookup klass "method_missing"
                with Not_found -> failwith ("no method " ^ (klass.Rt.k_name) ^ "#" ^ selector)
            in
            let lambda = imethod.Rt.im_body in
            let callee =
              (* Only translate each method once. *)
              match lookup_lambda capsule lambda with
              | Some callee
              -> callee
              | None
              -> (let callee = Ssa_gen.name_of_lambda klass selector lambda capsule in
                  Pass_manager.mark passmgr callee;
                  callee)
            in
            match Typing.instantiate (Rt.LambdaTy lambda.Rt.l_ty) with
            | Rt.LambdaTy (arg_tys, ret_ty)
            -> (let args = parse_arguments ~before:instr ~arg_tys ~args ~kwargs in
                let call = Ssa.create_instr ~location:instr.location
                                            ret_ty (Ssa.CallInstr (callee, args)) in
                replace_instr instr call)
            | _
            -> assert false))

    | PrimitiveInstr (prim, [callee; args; kwargs]) when prim = "lam_call"
    -> (match callee.ty with
        | Rt.LambdaTy (arg_tys, ret_ty)
        -> (let args = parse_arguments ~before:instr ~arg_tys ~args ~kwargs in
            let call = Ssa.create_instr ret_ty (Ssa.CallInstr (callee, args)) in
            replace_instr instr call)
        | _
        -> ())

    | _
    -> ())
