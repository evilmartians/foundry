open Unicode.Std
open Big_int
open Rt

exception Conflict of ty * ty

let fold_equiv ty =
  let roots = !roots in
  match ty with
  | Class (klass, specz) when klass == roots.kUnsigned
  -> (match Assoc.find_option specz "width" with
      | Some (Integer width) -> UnsignedTy (int_of_big_int width)
      | _ -> ty)
  | Class (klass, specz) when klass == roots.kSigned
  -> (match Assoc.find_option specz "width" with
      | Some (Integer width) -> SignedTy (int_of_big_int width)
      | _ -> ty)
  | _
  -> ty

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
  let a, b = fold_equiv a, fold_equiv b in
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
  -> Assoc.fold2 ~f:(fun _ -> unify') env xsa xsb
  | EnvironmentTy(xa), EnvironmentTy(xb)
  -> unify_env' env xa xb
  | Class(ka, spa), Class(kb, spb)
    when ka == kb
  -> Assoc.fold2 ~f:(fun _ -> unify') env spa spb
  | FunctionTy(args_ty, ret_ty), FunctionTy(args_ty', ret_ty')
  | ClosureTy(args_ty, ret_ty),  ClosureTy(args_ty', ret_ty')
  -> (try
        let env = List.fold_left2 unify' env args_ty args_ty' in
        unify' env ret_ty ret_ty'
      with Invalid_argument _ ->
        raise (Conflict (a, b)))
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

let unify = unify' []

let unify_list lst =
  match lst with
  | []
  -> []
  | [a]
  -> []
  | fst :: rest
  -> List.fold_left (fun env ty -> unify' env fst ty) [] rest

let rec subst env value =
  match value with
  | TvarTy | NilTy | BooleanTy | IntegerTy
  | UnsignedTy _ | SignedTy _ | SymbolTy
  | Nil | Truth | Lies | Integer _
  | Unsigned _ | Signed _ | Symbol _
  -> value
  | Tvar tvar
  -> (try  List.assoc tvar env
      with Not_found -> value)
  | TupleTy xs
  -> TupleTy (List.map (subst env) xs)
  | Tuple xs
  -> Tuple (List.map (subst env) xs)
  | RecordTy xs
  -> RecordTy (Assoc.map (fun _ -> subst env) xs)
  | Record xs
  -> Record (Assoc.map (fun _ -> subst env) xs)
  | Environment _
  -> value
  | EnvironmentTy x
  -> EnvironmentTy (subst_local_env env x)
  | Class (klass, specz)
  -> Class (klass, Assoc.map (fun _ -> subst env) specz)
  | Package _
  -> value
  | FunctionTy (args, ret)
  -> FunctionTy (List.map (subst env) args, subst env ret)
  | ClosureTy (args, ret)
  -> ClosureTy (List.map (subst env) args, subst env ret)
  | _
  -> failwith ("Typing.subst: " ^ (inspect_type value))

and subst_local_env env ty =
  { e_ty_parent   = Option.map (subst_local_env env) ty.e_ty_parent;
    e_ty_bindings = Table.map (fun binding ->
      { b_ty_location = binding.b_ty_location;
        b_ty_kind     = binding.b_ty_kind;
        b_ty          = subst env binding.b_ty;
      }) ty.e_ty_bindings }

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
