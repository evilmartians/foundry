open Unicode.Std

let rec match_ty' env pattern subject =
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
  -> (let env = match_tys' env args_ty args_ty' in
      match_ty' env ret_ty ret_ty')
  | a, b
  -> failwith ("Typing.match_ty' " ^ (Rt.inspect_type a) ^ " " ^ (Rt.inspect_type b))

and match_tys' env subjects patterns =
  match subjects, patterns with
  | subject :: subjects, pattern :: patterns
  -> (let env = match_ty' env subject pattern in
      match_tys' env subjects patterns)
  | [], []
  -> env
  | _, _
  -> assert false

let match_ty = match_ty' []

let rec rewrite env ty =
  match ty with
  | Rt.TvarTy | Rt.NilTy | Rt.BooleanTy | Rt.IntegerTy
  | Rt.UnsignedTy _ | Rt.SignedTy _ | Rt.SymbolTy
  -> ty
  | Rt.Tvar tvar
  -> (try  List.assoc tvar env
      with Not_found -> ty)
  | Rt.TupleTy (xs)
  -> Rt.TupleTy (List.map (rewrite env) xs)
  | Rt.FunctionTy (args, ret)
  -> Rt.FunctionTy (List.map (rewrite env) args, rewrite env ret)
