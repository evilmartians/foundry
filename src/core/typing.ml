open Unicode.Std

let rec unify_one env pattern subject =
  match pattern, subject with
  | Rt.Tvar a, Rt.Tvar b when a = b
  -> env
  | Rt.Tvar tvar, _
  -> (try
        let subject' = List.assoc tvar env in
        assert (subject' = subject);
        env
      with Not_found ->
        (tvar, subject) :: env)
  | Rt.FunctionTy(args_ty, ret_ty), Rt.FunctionTy(args_ty', ret_ty')
  -> (let env = unify_list env args_ty args_ty' in
      unify_one env ret_ty ret_ty')
  | Rt.UnsignedTy(wa), Rt.UnsignedTy(wb)
  | Rt.SignedTy(wa),   Rt.SignedTy(wb)
    when wa = wb
  -> env
  | a, b
  -> failwith ("Typing.unify_one " ^ (Rt.inspect_type a) ^ " " ^ (Rt.inspect_type b))

and unify_list env subjects patterns =
  match subjects, patterns with
  | subject :: subjects, pattern :: patterns
  -> (let env = unify_one env subject pattern in
      unify_list env subjects patterns)
  | [], []
  -> env
  | _, _
  -> assert false

let unify = unify_one []

let rec subst env ty =
  match ty with
  | Rt.TvarTy | Rt.NilTy | Rt.BooleanTy | Rt.IntegerTy
  | Rt.UnsignedTy _ | Rt.SignedTy _ | Rt.SymbolTy
  -> ty
  | Rt.Tvar tvar
  -> (try  List.assoc tvar env
      with Not_found -> ty)
  | Rt.TupleTy (xs)
  -> Rt.TupleTy (List.map (subst env) xs)
  | Rt.FunctionTy (args, ret)
  -> Rt.FunctionTy (List.map (subst env) args, subst env ret)
