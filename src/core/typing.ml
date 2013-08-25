open Unicode.Std
open Big_int
open Rt

exception Conflict of ty * ty

let fold_equiv ty =
  let roots = !roots in
  match ty with
  | Class (klass, specz) when klass == roots.kUnsigned
  -> (match Table.get specz "width" with
      | Some (Integer width) -> UnsignedTy (int_of_big_int width)
      | _ -> ty)
  | Class (klass, specz) when klass == roots.kSigned
  -> (match Table.get specz "width" with
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
  -> Table.fold2 ~f:(fun _ -> unify') env xsa xsb
  | EnvironmentTy(xa), EnvironmentTy(xb)
  -> unify_env' env xa xb
  | Class(ka, spa), Class(kb, spb)
    when ka == kb
  -> Table.fold2 ~f:(fun _ -> unify') env spa spb
  | FunctionTy(args_ty, ret_ty), FunctionTy(args_ty', ret_ty')
  -> (let env = List.fold_left2 unify' env args_ty args_ty' in
      unify' env ret_ty ret_ty')
  | ClosureTy(args_ty, ret_ty), ClosureTy(args_ty', ret_ty')
  -> (let env = List.fold_left2 unify' env args_ty args_ty' in
      unify' env ret_ty ret_ty')
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

let rec subst env ty =
  match ty with
  | TvarTy | NilTy | BooleanTy | IntegerTy
  | UnsignedTy _ | SignedTy _ | SymbolTy
  | Nil | Truth | Lies | Integer _
  | Unsigned _ | Signed _ | Symbol _
  -> ty
  | Tvar tvar
  -> (try  List.assoc tvar env
      with Not_found -> ty)
  | TupleTy xs
  -> TupleTy (List.map (subst env) xs)
  | Tuple xs
  -> Tuple (List.map (subst env) xs)
  | RecordTy xs
  -> RecordTy (Table.map (subst env) xs)
  | Record xs
  -> Record (Table.map (subst env) xs)
  | EnvironmentTy ty
  -> EnvironmentTy (subst_local_env env ty)
  | Class (klass, specz)
  -> Class (klass, Table.map (subst env) specz)
  | FunctionTy (args, ret)
  -> FunctionTy (List.map (subst env) args, subst env ret)
  | ClosureTy (args, ret)
  -> ClosureTy (List.map (subst env) args, subst env ret)
  | _
  -> failwith ("Typing.subst: " ^ (inspect_type ty))

and subst_local_env env ty =
  { e_ty_parent   = Option.map (subst_local_env env) ty.e_ty_parent;
    e_ty_bindings = Table.map (fun binding ->
      { b_ty_location = binding.b_ty_location;
        b_ty_kind     = binding.b_ty_kind;
        b_ty          = subst env binding.b_ty;
      }) ty.e_ty_bindings }

let print_env env =
  List.iter (fun (tvar, ty) ->
      print_endline ((inspect_type (Tvar tvar)) ^
                     " -> " ^ (inspect_type ty)))
    env

let rec slot_ty (klass, specz) name =
  (* Instance variable types and class parameters reside in one type variable
     "namespace" (that is, disjoint block), and specializations reside in another.
     They are linked by specializations named in the same way as class parameters,
     so if we unify their respective types, and then substitute tvars in instance
     variable type with the info we got, we'll get a fully qualified ivar type.

     This is probably very slow.
   *)
  let env = Table.fold [] specz ~f:(fun name env value ->
              unify' env value (Tvar (List.assoc name klass.k_parameters))) in
  try
    subst env (List.assoc name klass.k_slots).iv_ty
  with Not_found ->
    match klass.k_ancestor with
    | Some ancestor -> slot_ty (ancestor, specz) name
    | None -> failwith ("Typing.slot_ty: @" ^ name)
