open Unicode.Std

let match_ty env pattern subject =
  match pattern, subject with
  | Rt.Tvar _ as tvar, _
  -> (tvar, subject) :: env

let rec match_tys env subjects patterns =
  match subjects, patterns with
  | subject :: subjects, pattern :: patterns
  -> (let env = match_ty env subject pattern in
      match_tys env subjects patterns)
  | [], []
  -> env
  | _, _
  -> assert false

let rec rewrite env ty =
  match ty with
  | Rt.Tvar _ as tvar
  -> (try  List.assoc tvar env
      with Not_found -> tvar)
  | Rt.TupleTy (xs)
  -> Rt.TupleTy (List.map (rewrite env) xs)
  | Rt.FunctionTy (args, ret)
  -> Rt.FunctionTy (List.map (rewrite env) args, rewrite env ret)

let func_specialize ?args_ty ?return_ty func_ty =
  match func_ty with
  | Rt.FunctionTy (args_ty', return_ty')
  -> (let env = [] in
      let env = Option.map_default
          (match_tys env args_ty') env args_ty in
      let env = Option.map_default
          (match_ty env return_ty') env return_ty in
      rewrite env func_ty)
  | _
  -> assert false
