open Unicode.Std

exception Conflict of Rt.ty * Rt.ty

let rec unify_one env a b =
  (* Substitute already instantiated type variables. *)
  let rec subst_tvar ty =
    match ty with
    | Rt.Tvar tvar
    -> (try  subst_tvar (List.assoc tvar env)
        with Not_found -> ty)
    | _
    -> ty
  in
  let a, b = subst_tvar a, subst_tvar b in
  (* Instantiate type variables. *)
  match a, b with
  | _, _ when a = b
  -> env
  | Rt.Tvar tvar, ty
  | ty, Rt.Tvar tvar
  -> (try
        let subst = List.assoc tvar env in
        if not (subst = ty) then
          (*raise (Conflict (ty, subst));*)
          failwith ("Typing.unify_one subst=" ^ (Rt.inspect_type subst) ^
                    " <> ty=" ^ (Rt.inspect_type ty));
        env
      with Not_found ->
        (tvar, ty) :: env)
  | Rt.FunctionTy(args_ty, ret_ty), Rt.FunctionTy(args_ty', ret_ty')
  -> (let env = unify_list env args_ty args_ty' in
      unify_one env ret_ty ret_ty')
  | Rt.UnsignedTy(wa), Rt.UnsignedTy(wb)
  | Rt.SignedTy(wa),   Rt.SignedTy(wb)
    when wa = wb
  -> env
  | a, b
  -> failwith ("Typing.unify_one " ^ (Rt.inspect_type a) ^ " " ^ (Rt.inspect_type b))

and unify_list env a_s b_s =
  match a_s, b_s with
  | a :: a_s, b :: b_s
  -> (let env = unify_one env a b in
      unify_list env a_s b_s)
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
  | Rt.TupleTy xs
  -> Rt.TupleTy (List.map (subst env) xs)
  | Rt.RecordTy xs
  -> Rt.RecordTy (Table.map (subst env) xs)
  | Rt.EnvironmentTy ty
  -> Rt.EnvironmentTy (subst_local_env env ty)
  | Rt.FunctionTy (args, ret)
  -> Rt.FunctionTy (List.map (subst env) args, subst env ret)

and subst_local_env env ty =
  { Rt.e_parent_ty   = Option.map (subst_local_env env) ty.Rt.e_parent_ty;
    Rt.e_bindings_ty = Table.map (fun binding ->
      { Rt.b_location_ty = binding.Rt.b_location_ty;
        Rt.b_kind_ty     = binding.Rt.b_kind_ty;
        Rt.b_value_ty    = subst env binding.Rt.b_value_ty;
      }) ty.Rt.e_bindings_ty }
