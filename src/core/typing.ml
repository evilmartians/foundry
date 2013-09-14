open Unicode.Std
open Big_int
open Rt

exception Conflict of ty * ty

let fold_equiv ty =
  let roots = !roots in
  match ty with
  | Class (klass, _) when klass == roots.kNil
  -> Rt.NilTy
  | Class (klass, _) when klass == roots.kBoolean
  -> Rt.BooleanTy
  | Class (klass, _) when klass == roots.kInteger
  -> Rt.IntegerTy
  | Class (klass, _) when klass == roots.kSymbol
  -> Rt.SymbolTy
  | Class (klass, _) when klass == roots.kString
  -> Rt.StringTy
  | Class (klass, specz) when klass == roots.kFixed
  -> (match Assoc.find_option specz "width" with
      | Some (Integer width)
      -> (match Assoc.find_option specz "signed" with
          | Some Lies  -> UnsignedTy (int_of_big_int width)
          | Some Truth -> SignedTy (int_of_big_int width)
          | _ -> ty)
      | _ -> ty)
  | _
  -> ty

let unfold_equiv ty =
  match ty with
  | Class (k, sp)
  -> k, sp
  | UnsignedTy (width)
  -> (let width = big_int_of_int width in
      klass_of_type ty, Assoc.sorted ["width", Rt.Integer (width); "signed", Rt.Lies])
  | SignedTy (width)
  -> (let width = big_int_of_int width in
      klass_of_type ty, Assoc.sorted ["width", Rt.Integer (width); "signed", Rt.Truth])
  | _ -> raise (Invalid_argument ("Typing.unfold_equiv" :> latin1s))

let equiv ~f ty =
  fold_equiv (f (unfold_equiv ty))

let rec unify' env a b =
  (* Substitute already instantiated type variables. *)
  let rec subst_tvar ty =
    match ty with
    | Tvar tvar
    -> (try  subst_tvar (List.assoc tvar env)
        with Not_found -> ty)
    | _
    -> ty
  in
  let a, b = subst_tvar a, subst_tvar b in

  (* Instantiate type variables. *)
  match a, b with
  | _, _ when equal a b
  -> env

  | Tvar tvar, ty
  | ty, Tvar tvar
  -> (try
        let subst = List.assoc tvar env in
        if not (subst = ty) then
          raise (Conflict (ty, subst));
        env
      with Not_found ->
        (tvar, ty) :: env)

  | UnsignedTy(wa), UnsignedTy(wb)
  | SignedTy(wa),   SignedTy(wb)
    when wa = wb
  -> env

  | TupleTy(xsa), TupleTy(xsb)
  -> List.fold_left2 unify' env xsa xsb

  | RecordTy(xsa), RecordTy(xsb)
  -> fst (Assoc.merge_fold ~f:(fun k env v1 v2 -> unify' env v1 v2, v1) env xsa xsb)

  | EnvironmentTy(xa), EnvironmentTy(xb)
  -> unify_env' env xa xb

  | Class(ka, spa), Class(kb, spb)
    when ka == kb
  -> fst (Assoc.merge_fold ~f:(fun k env v1 v2 -> unify' env v1 v2, v1) env spa spb)

  | LambdaTy(args_ty, ret_ty),   LambdaTy(args_ty', ret_ty')
  -> (try
        let env = List.fold_left2 unify_lambda_ty_elem' env args_ty args_ty' in
        unify' env ret_ty ret_ty'
      with Invalid_argument _ ->
        raise (Conflict (a, b)))

  | FunctionTy(args_ty, ret_ty), FunctionTy(args_ty', ret_ty')
  -> (try
        let env = List.fold_left2 unify' env args_ty args_ty' in
        unify' env ret_ty ret_ty'
      with Invalid_argument _ ->
        raise (Conflict (a, b)))

  (* TODO: refactor this? *)
  | (Class(_) as a),      (UnsignedTy(_) as b)
  | (UnsignedTy(_) as b), (Class(_) as a)
  | (Class(_) as a),      (SignedTy(_) as b)
  | (SignedTy(_) as b),   (Class(_) as a)
  -> unify' env a (Class (unfold_equiv b))

  | a, b
  -> raise (Conflict (a, b))

and unify_env' env a b =
  let env =
    match a.e_ty_parent, b.e_ty_parent with
    | None,   None   -> env
    | Some a, Some b -> unify_env' env a b
    | _, _ -> assert false
  in
  Table.fold2 ~f:(fun _ env a b ->
      assert (a.b_ty_kind = b.b_ty_kind);
      unify' env a.b_ty b.b_ty) env
    a.e_ty_bindings b.e_ty_bindings

and unify_lambda_ty_elem' env a b =
  match a, b with
  | LambdaArg a,      LambdaArg b
  | LambdaOptArg a,   LambdaOptArg b
  | LambdaRest a,     LambdaRest b
  | LambdaKwRest a,   LambdaKwRest b
  -> unify' env a b
  | LambdaKwArg (ka,a),    LambdaKwArg (kb,b)
  | LambdaKwOptArg (ka,a), LambdaKwOptArg (kb,b)
    when ka = kb
  -> unify' env a b
  | _
  -> invalid_arg "unify_lambda_ty_elem'"

let unify = unify' []

let unify_list lst =
  match lst with
  | []
  -> []
  | [a]
  -> []
  | fst :: rest
  -> List.fold_left (fun env ty -> unify' env fst ty) [] rest

let rec derive f value =
  match value with
  | TvarTy | NilTy | BooleanTy | IntegerTy
  | UnsignedTy _ | SignedTy _ | SymbolTy
  | Nil | Truth | Lies | Integer _
  | Unsigned _ | Signed _ | Symbol _ | String _
  -> value
  | Tvar tvar
  -> f tvar
  | TupleTy xs
  -> TupleTy (List.map (derive f) xs)
  | Tuple xs
  -> Tuple (List.map (derive f) xs)
  | RecordTy xs
  -> RecordTy (Assoc.map (fun _ -> derive f) xs)
  | Record xs
  -> Record (Assoc.map (fun _ -> derive f) xs)
  | Environment _
  -> value
  | EnvironmentTy x
  -> EnvironmentTy (derive_local_env_ty f x)
  | Class (klass, specz)
  -> fold_equiv (Class (klass, Assoc.map (fun _ -> derive f) specz))
  | Package _
  -> value
  | Instance _
  -> value
  | LambdaTy (args, ret)
  -> (let derive_arg arg =
        match arg with
        | LambdaArg       ty      -> LambdaArg (derive f ty)
        | LambdaOptArg    ty      -> LambdaOptArg (derive f ty)
        | LambdaRest      ty      -> LambdaRest (derive f ty)
        | LambdaKwArg    (kw, ty) -> LambdaKwArg (kw, derive f ty)
        | LambdaKwOptArg (kw, ty) -> LambdaKwOptArg (kw, derive f ty)
        | LambdaKwRest    ty      -> LambdaKwRest (derive f ty)
      in
      LambdaTy (List.map derive_arg args, derive f ret))
  | FunctionTy (args, ret)
  -> FunctionTy (List.map (derive f) args, derive f ret)
  | _
  -> failwith ("Typing.derive: " ^ (inspect_type value))

and derive_local_env_ty f ty =
  { e_ty_parent   = Option.map (derive_local_env_ty f) ty.e_ty_parent;
    e_ty_bindings = Table.map (fun binding ->
      { b_ty_location = binding.b_ty_location;
        b_ty_kind     = binding.b_ty_kind;
        b_ty          = derive f binding.b_ty;
      }) ty.e_ty_bindings }

let rec subst env value =
  derive (fun tvar ->
      try  subst env (List.assoc tvar env)
      with Not_found -> Rt.Tvar tvar)
    value

let instantiate value =
  let tvar_map = ref [] in
  derive (fun tvar ->
      try
        List.assoc tvar !tvar_map
      with Not_found ->
        let new_tvar = Rt.Tvar (Rt.new_tvar ()) in
        tvar_map := (tvar, new_tvar) :: !tvar_map;
        new_tvar)
    value

let print_env env =
  List.iter (fun (tvar, ty) ->
      prerr_endline ((inspect_type (Tvar tvar)) ^
                     " -> " ^ (inspect_type ty)))
    env

let meaningful env =
  List.filter (fun (tv, ty) ->
      match ty with
      | Rt.Tvar _ -> false
      | _ -> true)
    env

let rec slot_ty (klass, specz) name =
  (* Instance variable types and class parameters reside in one type variable
     "namespace" (that is, disjoint block), and specializations reside in another.
     They are linked by specializations named in the same way as class parameters,
     so if we unify their respective types, and then substitute tvars in instance
     variable type with the info we got, we'll get a fully qualified ivar type.

     This is probably very slow.
   *)
  let env = Assoc.fold [] specz ~f:(fun name env value ->
              unify' env value (Tvar (Assoc.find klass.k_parameters name))) in
  try
    subst env (Assoc.find klass.k_ivars name).iv_ty
  with Not_found ->
    match klass.k_ancestor with
    | Some ancestor -> slot_ty (ancestor, specz) name
    | None -> failwith ("Typing.slot_ty: @" ^ name)
