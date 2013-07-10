open Sexplib.Std
open Unicode.Std
open Rt

(* Local environment *)

let lenv_create parent : local_env =
  { e_parent   = parent;
    e_bindings = Table.create [] }

let lenv_bind env name ~loc ~is_mutable ~value =
  match Table.get env.e_bindings name with
  | Some(b)
  -> assert false
  | None
  -> Table.set env.e_bindings name {
    b_location   = loc;
    b_is_mutable = is_mutable;
    b_value      = value;
  }

let rec lenv_mutate env name ~value =
  match Table.get env.e_bindings name with
  | Some({ b_is_mutable = false })
  -> assert false
  | Some b
  -> Table.set env.e_bindings name {
    b_location   = b.b_location;
    b_is_mutable = true;
    b_value      = value;
  }
  | None
  -> match env.e_parent with
     | Some parent -> lenv_mutate parent name value
     | None -> assert false

let rec lenv_lookup env name =
  match Table.get env.e_bindings name with
  | Some({ b_value = value }) -> value
  | None
  -> match env.e_parent with
     | Some parent -> lenv_lookup parent name
     | None -> assert false

(* Type environment *)

let tenv_create () : type_env =
  Table.create []

let tenv_fork env =
  Table.copy env

let tenv_resolve env name =
  match Table.get env name with
  | Some tvar -> tvar
  | None ->
    let tvar = Rt.genvar () in
      Table.set env name tvar;
      tvar

(* Constant environment *)

exception CEnvUnbound
exception CEnvAlreadyBound of value

let cenv_create () : const_env =
  ref [!roots.pToplevel]

let cenv_fork env =
  ref !env

let cenv_extend env pkg =
  env := pkg :: !env

let cenv_bind env name value =
  let pkg = List.hd !env in
    match Table.get pkg.p_constants name with
    | Some value -> raise (CEnvAlreadyBound value)
    | None -> Table.set pkg.p_constants name value

let cenv_peek env name =
  let pkg = List.hd !env in
    Table.get pkg.p_constants name

let cenv_lookup env name =
  let rec lookup lst =
    match lst with
    | pkg :: rest
    -> (match Table.get pkg.p_constants name with
        | Some value -> value
        | None -> lookup rest)
    | []
    -> raise CEnvUnbound

  in lookup !env

(* Eval helper routines *)

type env = local_env * type_env * const_env

let env_create () =
  let lenv = lenv_create None
  in lenv_bind lenv "self" ~value:(Package !roots.pToplevel)
                           ~is_mutable:false
                           ~loc:Location.empty;
     lenv, tenv_create (), cenv_create ()

let concat_tuple lhs rhs =
  match lhs, rhs with
  | Tuple(l), Tuple(r)
  -> Tuple(l @ r)
  | _ -> assert false

let concat_record lhs rhs =
  match lhs, rhs with
  | Record(l), Record(r)
  -> Record(Table.join l r)
  | _ -> assert false

let check_class lenv loc =
  match lenv_lookup lenv "self" with
  | Class (klass,_) -> klass
  | value -> exc_type "class" value [loc]

(* E V A L *)

let rec eval_tuple env elem =
  match elem with
  | Syntax.TupleElem(_,expr)
  -> Tuple [eval_expr env expr]

  | Syntax.TupleSplice(_,expr)
  -> (match (eval_expr env expr) with
      | Tuple(_) as t -> t
      | o -> exc_type "Tuple" o [Syntax.loc expr])

and eval_record env elem =
  match elem with
  | Syntax.RecordElem(_,k,v)
  -> Record (Table.pair k (eval_expr env v))

  | Syntax.RecordSplice(_,expr)
  -> (match (eval_expr env expr) with
     | Record(_) as r -> r
     | o -> exc_type "Record" o [Syntax.loc expr])

  | Syntax.RecordPair(_,k,v)
  -> (match (eval_expr env k) with
     | Symbol(s) -> Record (Table.pair s (eval_expr env v))
     | o -> exc_type "Symbol" o [Syntax.loc k])

and eval_pattern ((lenv, tenv, cenv) as env) lhs value =
  match lhs with
  | Syntax.PatImmutable((loc,_),name)
  -> lenv_bind lenv name ~is_mutable:false ~value ~loc
  | Syntax.PatMutable((loc,_),name)
  -> lenv_bind lenv name ~is_mutable:true ~value ~loc
  | Syntax.PatTuple((loc,_),pats)
  -> (match value with
      | Tuple(xs)
      -> (if List.length(xs) = List.length(pats) then
            List.iter2 (eval_pattern env) pats xs
          else
            exc_fail ("Tuple " ^ (inspect value) ^ " of length " ^
                      (string_of_int (List.length xs)) ^
                      " does not match pattern of length " ^
                      (string_of_int (List.length pats))) [loc])
      | o -> exc_type "Tuple" o [Syntax.pat_loc lhs])
  | _ -> assert false

and eval_assign (lenv, tenv, cenv) lhs value =
  match lhs with
  | Syntax.Var((loc,_),name)
  -> lenv_mutate lenv name ~value:value
  | Syntax.Const((loc,_),name)
  -> (try
        cenv_bind cenv name value
      with CEnvAlreadyBound(value) ->
        exc_fail ("Name " ^ name ^ " is already bound to " ^ (inspect value)) [loc])
  | _ -> assert false

and eval_type ((lenv, tenv, cenv) as env) expr =
  let as_type expr =
    let ty = eval_type env expr in
      match ty with
      | Tvar(_) | TvarTy | NilTy | BooleanTy
      | IntegerTy | SymbolTy
      | TupleTy(_) | RecordTy(_) | LambdaTy(_)
      | Class(_,_)
      -> ty
      | _ -> exc_type "type" ty [Syntax.ty_loc expr]
  in
  match expr with
  | Syntax.TypeVar(_,name)
  -> Tvar (tenv_resolve tenv name)
  | Syntax.TypeTuple(_,xs)
  -> TupleTy (List.map as_type xs)
  | Syntax.TypeRecord(_,xs)
  -> RecordTy (Table.create (List.map (fun (_,k,v) -> k, as_type v) xs))
  | Syntax.TypeFunction(_,all_args,ret)
  -> (let args, kwargs =
        List.fold_left (fun (args, kwargs) arg ->
          match arg with
          | Syntax.TypeArg(_,ty)     -> ((as_type ty) :: args), kwargs
          | Syntax.TypeArgKw(_,n,ty) -> args, (n, (as_type ty)) :: kwargs)
        ([], []) all_args
      in LambdaTy {
        l_args_ty   = TupleTy  args;
        l_kwargs_ty = RecordTy (Table.create kwargs);
        l_return_ty = as_type ret;
      })
  | Syntax.TypeConstr((loc,_),name,args)
  -> (try
        match cenv_lookup cenv name with
        | (NilTy | BooleanTy | IntegerTy | SymbolTy | TvarTy) as ty
        -> (match args with
            | [] -> ty
            | _  -> exc_fail ("Type " ^ name ^ " is not parametric") [loc])
        | Class(klass,specz)
        -> (let new_specz =
              Table.create (List.map
                (fun ((loc,_), name, expr) ->
                  if Table.exists klass.k_tvars name then
                    name, eval_type env expr
                  else
                    exc_fail ("Type " ^ klass.k_name ^
                              " is not parametric by " ^ name) [loc]) args)
            in Class(klass, Table.join specz new_specz))
        | value
        -> exc_fail ("Name " ^ name ^ " is bound to " ^ (inspect value) ^
                     " which is not a type") [loc]
      with CEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is unbound") [loc])
  | Syntax.TypeSplice(_,expr)
  -> eval_expr env expr

and eval_closure_ty (lenv, tenv, cenv) expr =
  Option.map_default (fun ty_expr ->
    let tenv = tenv_fork tenv in
      let ty = eval_type (lenv, tenv, cenv) ty_expr in
        match ty with
        | LambdaTy(_) | Tvar(_)
        -> tenv, ty
        | _
        -> exc_type "closure type" ty [Syntax.ty_loc ty_expr])
    (tenv, Tvar (genvar ()))
    expr

and eval_expr ((lenv, tenv, cenv) as env) expr =
  match expr with
  | Syntax.Nil(_)   -> Nil
  | Syntax.Truth(_) -> Truth
  | Syntax.Lies(_)  -> Lies
  | Syntax.Int(_,x) -> Int(x)
  (* | Syntax.Sym(_,x) -> Symbol(x) *)
  | Syntax.Tuple(_,xs)
  -> List.fold_left concat_tuple
        (Tuple []) (List.map (eval_tuple env) xs)
  | Syntax.Record(_,xs)
  -> List.fold_left concat_record
        (Record (Table.create [])) (List.map (eval_record env) xs)
  | Syntax.Type(_,ty_expr)
  -> eval_type (lenv, (tenv_fork tenv), cenv) ty_expr
  | Syntax.Let(_,pat,_ty,expr)
  -> (let value = eval_expr env expr in
        eval_pattern env pat value;
        value)
  | Syntax.Var(loc,name)
  -> lenv_lookup lenv name
  | Syntax.Self(loc)
  -> lenv_lookup lenv "self"
  | Syntax.Const(loc,name)
  -> (try
        cenv_lookup cenv name
      with CEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") [Syntax.loc expr])
  | Syntax.Assign(_,lhs,rhs)
  -> (let value = eval_expr env rhs in
        eval_assign env lhs value;
        value)
  | Syntax.Lambda(_,args,ty_expr,body)
  -> (let tenv, ty = eval_closure_ty env ty_expr in
        Lambda {
          l_location  = Syntax.loc expr;
          l_ty        = ty;
          l_local_env = lenv;
          l_type_env  = tenv;
          l_args      = args;
          l_code      = [body]
        })
  | Syntax.Class((loc,_),name,ancestor,body)
  -> (let ancestor, specz =
        (* Extract ancestor class object and ancestor specialization table *)
        match ancestor with
        | Some expr
        -> (match eval_expr env expr with
            | Class (klass,specz) -> Some klass, Some specz
            | value -> exc_type "inheritable class" value [Syntax.loc expr])
        | None
        -> None, None
      in
      (* Check if we should extend existing class, or create a new one
         and bind it *)
      let klass =
        match cenv_peek cenv name with
        (* There's an existing one, and it is compatible *)
        | Some (Class (klass,_) as value) when klass.k_ancestor = ancestor
        -> value
        | Some (Class (klass,_) as value) when ancestor = None
        -> value
        (* There's an existing one, and it is not compatible with
           the present definition *)
        | Some (Class (klass,_))
        -> (let inspect_ancestor ancestor =
              match ancestor with
              | Some klass -> "has ancestor " ^ klass.k_name
              | None -> "does not have an ancestor"
            in
              exc_fail ("Cannot reopen " ^ name ^ ": it " ^
                        (inspect_ancestor klass.k_ancestor) ^
                        ", and the definition " ^
                        (inspect_ancestor ancestor)) [loc]) (* TODO loc *)
        (* Not a class *)
        | Some value
        -> exc_fail ("Cannot reopen " ^ name ^ ": it is bound to " ^
                     (inspect value) ^ ", which is not a class") [loc]
        (* No class present, create one and inherit specializations from
           its ancestor *)
        | None
        -> (let specz = Option.map_default Table.copy (Table.create []) specz
            in let value = Class (new_class ?ancestor name, specz)
               in cenv_bind cenv name value; value)
      in
      (* Evaluate class body in a context where self is bound to the class *)
      let lenv = lenv_create (Some lenv) in
        lenv_bind lenv "self" ~value:klass ~is_mutable:false ~loc:loc;
          eval (lenv, tenv, cenv) body)
  | Syntax.DefMethod((loc,_),name,args,ty_expr,body)
  -> (let klass = check_class lenv loc in
      match Table.get klass.k_methods name with
      | Some meth
      -> exc_fail ("Cannot define method " ^ name ^ " on " ^
                   (inspect_value (lenv_lookup lenv "self")) ^
                   ": it is already defined") [loc; meth.im_body.l_location]
      | None
      -> (let tenv, ty = eval_closure_ty env ty_expr in
          Table.set klass.k_methods name {
            im_dynamic  = false; (* TODO dynamic *)
            im_body     = {
              l_location  = loc;
              l_ty        = ty;
              l_local_env = lenv;
              l_type_env  = tenv;
              l_args      = args;
              l_code      = body;
            }
          });
      Nil)
  | Syntax.DefIVar((loc,_),name,kind,ty_expr)
  -> (let klass = check_class lenv loc in
      match Table.get klass.k_ivars name with
      | Some ivar
      -> exc_fail ("Cannot define @" ^ name ^ " on " ^
                   (inspect_value (lenv_lookup lenv "self")) ^
                   ": it is already defined with type " ^
                   (inspect_type ivar.iv_ty)) [loc; ivar.iv_location]
      | None
      -> (Table.set klass.k_ivars name {
            iv_location = loc;
            iv_ty       = eval_type env ty_expr;
            iv_kind     = kind;
          });
      Nil)
  | Syntax.InvokePrimitive(_,name,args)
  -> (let args = List.map (eval_expr env) args in
        Primitive.invoke name args)
  | _
  -> failwith ("cannot eval " ^
               (Unicode.assert_utf8s
                (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr))));

and eval env exprs =
  Option.default Nil
    (List.fold_left (fun _ expr -> Some (eval_expr env expr)) None exprs)
