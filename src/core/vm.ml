open Sexplib.Std
open Unicode.Std
open ExtList
open Rt

(* Eval helper routines *)

type env = local_env * type_env * const_env ref

let env_create () =
  let lenv = lenv_create None
  in lenv_bind lenv "self" ~value:(Package !roots.pToplevel)
                           ~kind:Syntax.LVarImmutable
                           ~loc:Location.empty;
  lenv, tenv_create (), ref (cenv_create ())

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

let define_method obj name body loc =
  match obj with
  | Class (klass,_)
  -> (try
        let meth = List.assoc name klass.k_methods in
        exc_fail ("Cannot define method " ^ name ^ " on " ^ (inspect_value obj) ^
                  ": it is already defined") [loc; meth.im_body.l_location]
      with Not_found ->
        klass.k_methods <- klass.k_methods @ [name, body])
  | value -> exc_type "Class" value [loc]

let define_ivar obj name body loc =
  match obj with
  | Class (klass,_)
  -> (try
        let ivar = List.assoc name klass.k_slots in
        exc_fail ("Cannot define @" ^ name ^ " on " ^
                  (inspect_value obj) ^
                  ": it is already defined with type " ^
                  (inspect_type ivar.iv_ty)) [loc; ivar.iv_location]
      with Not_found ->
        klass.k_slots <- klass.k_slots @ [name, body])
  | value -> exc_type "Class" value [loc]

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
  | Syntax.PatVariable((loc,_),(kind,name))
  -> lenv_bind lenv name ~kind ~value ~loc

  | Syntax.PatTuple((loc,_),pats)
  -> (match value with
      | Tuple(xs)
      -> (if List.length(xs) = List.length(pats) then
            List.iter2 (eval_pattern env) pats xs
          else
            exc_fail ("Tuple " ^ (inspect value) ^ " of length " ^
                      (string_of_int (List.length xs)) ^
                      " does not match pattern of length " ^
                      (string_of_int (List.length pats)) ^ ".") [loc])
      | o -> exc_type "Tuple" o [Syntax.pat_loc lhs])

  | _ -> assert false

and eval_assign (lenv, tenv, cenv) lhs value =
  match lhs with
  | Syntax.Var((loc,_),name)
  -> lenv_mutate lenv name ~value:value
  | Syntax.Const((loc,_),name)
  -> (try
        cenv_bind !cenv name value
      with CEnvAlreadyBound(value) ->
        exc_fail ("Name " ^ name ^ " is already bound to " ^ (inspect value) ^ ".") [loc])
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
  | Syntax.TypeFunction(_, args, ret)
  -> (let args, kwargs =
        List.fold_left (fun (args, kwargs) arg ->
          match arg with
          | Syntax.TypeArg(_,ty)     -> ((as_type ty) :: args), kwargs
          | Syntax.TypeArgKw(_,n,ty) -> args, (n, as_type ty) :: kwargs)
        ([], []) args
      in LambdaTy {
        l_ty_args   = TupleTy  args;
        l_ty_kwargs = RecordTy (Table.create kwargs);
        l_ty_result = as_type ret;
      })
  | Syntax.TypeConstr((loc, _), name, args)
  -> (try
        match cenv_lookup !cenv name with
        | (NilTy | BooleanTy | IntegerTy | SymbolTy | TvarTy) as ty
        -> (match args with
            | [] -> ty
            | _  -> exc_fail ("Type " ^ name ^ " is not parametric") [loc])
        | Class (klass, specz)
        -> (let args, kw_args =
              List.fold_left (fun (args, kwargs) arg ->
                match arg with
                | Syntax.TypeArg(_, value)
                -> (eval_type env value) :: args, kwargs
                | Syntax.TypeArgKw(_, name, value)
                -> args, (name, eval_type env value) :: kwargs)
              ([], []) args
            in
            let f_args, kw_f_args =
              List.split_nth (List.length args) (List.map fst klass.k_parameters)
            in
            let new_specz = List.combine f_args args in
            let new_specz = List.fold_left (fun acc (kw, ty) ->
                if List.mem_assoc kw new_specz then
                  exc_fail ("Type parameter `" ^ kw ^ "' is passed more than once") [loc]
                else if not (List.mem_assoc kw klass.k_parameters) then
                  exc_fail ("Class `" ^ klass.k_name ^ "' is not parametric by `" ^ kw) [loc]
                else
                  (kw, ty) :: acc) new_specz kw_args
            in
            Class (klass, Table.join specz (Table.create new_specz)))
        | value
        -> exc_fail ("Name " ^ name ^ " is bound to " ^ (inspect value) ^
                     " which is not a type") [loc]
      with CEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is unbound") [loc])
  | Syntax.TypeSplice(_,expr)
  -> eval_expr env expr

and eval_closure_ty (lenv, tenv, cenv) expr =
  match expr with
  | Some ty_expr
  -> (let tenv = tenv_fork tenv in
        let ty = eval_type (lenv, tenv, cenv) ty_expr in
          match ty with
          | LambdaTy(ty)
          -> tenv, ty
          | _
          -> exc_type "closure type" ty [Syntax.ty_loc ty_expr])
  | None
  -> (tenv, {
        l_ty_args   = Tvar (new_tvar ());
        l_ty_kwargs = Tvar (new_tvar ());
        l_ty_result = Tvar (new_tvar ());
      })

and eval_args env lst =
  let rec eval_args args =
    match args with
    | Syntax.ActualArg(_,arg) :: rest
    -> eval_expr env arg :: eval_args rest

    | Syntax.ActualSplice(_,arg) :: rest
    -> (match (eval_expr env arg) with
        | Tuple(t) -> t @ eval_args rest
        | o        -> exc_type "Tuple" o [Syntax.loc arg])

    | _ :: rest -> eval_args rest
    | []        -> []
  in
  let rec eval_kwargs args table =
    match args with
    | Syntax.ActualKwArg(_,k,v) :: rest
    -> Table.set table k (eval_expr env v);
       eval_kwargs rest table

    | Syntax.ActualKwSplice(_,expr) :: rest
    -> (match (eval_expr env expr) with
        | Record(r) -> eval_kwargs rest (Table.join table r)
        | o         -> exc_type "Record" o [Syntax.loc expr])

    | Syntax.ActualKwPair(_,k,v) :: rest
    -> (match (eval_expr env k) with
        | Symbol(k) -> Table.set table k (eval_expr env v);
                       eval_kwargs rest table
        | o         -> exc_type "Symbol" o [Syntax.loc k])

    | _ :: rest -> eval_kwargs rest table
    | []        -> table
  in
  eval_args lst, eval_kwargs lst (Table.create [])

and eval_expr ((lenv, tenv, cenv) as env) expr =
  match expr with
  | Syntax.Nil(_)   -> Nil
  | Syntax.Truth(_) -> Truth
  | Syntax.Lies(_)  -> Lies
  | Syntax.Integer(_,x) -> Integer(x)
  | Syntax.Symbol(_,x) -> Symbol(x)

  | Syntax.Unsigned(_,w,x) -> Unsigned(w,x)
  | Syntax.Signed(_,w,x)   -> Signed(w,x)

  | Syntax.Tuple(_, xs)
  -> List.fold_left concat_tuple
        (Tuple []) (List.map (eval_tuple env) xs)

  | Syntax.Record(_, xs)
  -> List.fold_left concat_record
        (Record (Table.create [])) (List.map (eval_record env) xs)

  | Syntax.Type(_, ty_expr)
  -> eval_type (lenv, (tenv_fork tenv), cenv) ty_expr

  | Syntax.Let(_, pat, ty, expr)
  -> (let value = eval_expr env expr in
        eval_pattern env pat value;
        value)

  | Syntax.Var(loc, name)
  -> lenv_lookup lenv name

  | Syntax.Self(loc)
  -> lenv_lookup lenv "self"

  | Syntax.Const(loc, name)
  -> (try
        cenv_lookup !cenv name
      with CEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") [Syntax.loc expr])

  | Syntax.Assign(_, lhs, rhs)
  -> (let value = eval_expr env rhs in
        eval_assign env lhs value;
        value)

  | Syntax.OpAssign((loc, _), lhs, meth, rhs)
  -> (let value  = eval_expr env lhs in
      let arg    = eval_expr env rhs in
      let result = eval_send value meth ~args:[arg] ~kwargs:(Table.create []) ~loc in
        eval_assign env lhs result;
        result)

  | Syntax.Lambda(_, args, ty_expr, body)
  -> (let tenv, ty = eval_closure_ty env ty_expr in
        Lambda {
          l_hash      = Hash_seed.make ();
          l_location  = Syntax.loc expr;
          l_ty        = ty;
          l_local_env = lenv;
          l_type_env  = tenv;
          l_const_env = !cenv;
          l_args      = args;
          l_body      = [body]
        })

  | Syntax.Class((loc,_), name, params, ancestor, body)
  -> (let ancestor, specz =
        (* Extract ancestor class object and ancestor specialization table. *)
        match ancestor with
        | Some expr
        -> (match eval_expr env expr with
            | Class (klass, specz) -> Some klass, Some specz
            | value -> exc_type "inheritable class" value [Syntax.loc expr])
        | None
        -> None, None
      in
      (* Check if we should extend existing class, or create a new one
         and bind it. *)
      let klass =
        match cenv_peek !cenv name with
        (* There's an existing one, and it is compatible *)
        | Some(Class (klass, _) as value)
          when klass.k_ancestor = ancestor && params = []
        -> value
        | Some(Class (klass, _) as value)
          when ancestor = None && params = []
        -> value
        (* There's an existing one, and it is not compatible with
           the present definition. *)
        | Some(Class (klass, _))
        -> (let inspect_ancestor ancestor =
              match ancestor with
              | Some klass -> "has ancestor " ^ klass.k_name
              | None -> "does not have an ancestor"
            in
              exc_fail ("Cannot reopen " ^ name ^ ": it " ^
                        (inspect_ancestor klass.k_ancestor) ^
                        ", and the definition " ^
                        (inspect_ancestor ancestor)) [loc]) (* TODO loc *)
        (* Special classes. *)
        | Some(TvarTy as value)         | Some(BooleanTy as value)
        | Some(NilTy as value)          | Some(IntegerTy as value)
        | Some(SymbolTy as value)       | Some(TupleTy(_) as value)
        | Some(RecordTy(_) as value)    | Some(LambdaTy(_) as value)
        | Some(UnsignedTy(_) as value)  | Some(SignedTy(_) as value)
        -> (match ancestor with
            | Some klass
            -> exc_fail ("Cannot reopen internal class " ^ name ^ " with an ancestor.") [loc]
            | None
            -> Class (klass_of_value value, Table.create []))
        (* Not a class. *)
        | Some value
        -> exc_fail ("Cannot reopen " ^ name ^ ": it is bound to " ^
                     (inspect value) ^ ", which is not a class") [loc]
        (* No class present, create one and inherit specializations from
           its ancestor. *)
        | None
        -> (let eval_param arg =
              match arg with
              | Syntax.FormalTypeArg (_, name)
              -> name, tenv_resolve tenv name
              | Syntax.FormalTypeKwArg (_, kw, name)
              -> kw,   tenv_resolve tenv name
            in
            let parameters = List.map eval_param params in
            let specz      = Option.map_default Table.copy (Table.create []) specz in
            let ancestor   = Option.default !roots.kObject ancestor in
            let value      = Class (new_class ~ancestor ~parameters name, specz) in
            cenv_bind !cenv name value; value)
      in
      (* Evaluate class body in a context where self is bound to the class *)
      let lenv = lenv_create (Some lenv) in
        lenv_bind lenv "self" ~value:klass ~kind:Syntax.LVarImmutable ~loc;
          eval (lenv, tenv, cenv) body)

  | Syntax.DefMethod((loc,_),name,args,ty_expr,body)
  -> (let definee  =
        match lenv_lookup lenv "self" with
        | Package(p) when p == !roots.pToplevel
        -> Class(p.p_metaclass, Table.create [])
        | other
        -> other
      in
      let tenv, ty = eval_closure_ty env ty_expr in
        define_method definee name {
          im_hash     = Hash_seed.make ();
          im_dynamic  = false;
          im_body     = {
            l_hash      = Hash_seed.make ();
            l_location  = loc;
            l_ty        = ty;
            l_local_env = lenv;
            l_type_env  = tenv;
            l_const_env = !cenv;
            l_args      = args;
            l_body      = body;
          }
        } loc;
      Nil)

  | Syntax.DefSelfMethod((loc,_),name,args,ty_expr,body)
  -> (let tenv, ty = eval_closure_ty env ty_expr in
      let definee  = Class (klass_of_value ~dispatch:true (lenv_lookup lenv "self"),
                            Table.create []) in
        define_method definee name {
          im_hash     = Hash_seed.make ();
          im_dynamic  = false;
          im_body     = {
            l_hash    = Hash_seed.make ();
            l_location  = loc;
            l_ty        = ty;
            l_local_env = lenv;
            l_type_env  = tenv;
            l_const_env = !cenv;
            l_args      = args;
            l_body      = body;
          }
        } loc;
      Nil)

  | Syntax.DefIVar((loc,_),name,kind,ty_expr)
  -> (define_ivar (lenv_lookup lenv "self") name {
        iv_hash     = Hash_seed.make ();
        iv_location = loc;
        iv_ty       = eval_type env ty_expr;
        iv_kind     = kind;
      } loc; Nil)

  | Syntax.InvokePrimitive(_,name,args)
  -> (let args = List.map (eval_expr env) args in
        Primitive.invoke name args)

  | Syntax.Send((_, { Syntax.selector = loc }),recv,name,args)
  -> (let recv = eval_expr env recv in
      let args, kwargs = eval_args env args in
        eval_send recv name ~args ~kwargs ~loc)

  | _
  -> failwith ("cannot eval " ^
               (Unicode.assert_utf8s
                (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr))));

and eval_send recv name ~args ~kwargs ~loc =
  let method_table = (klass_of_value ~dispatch:true recv).k_methods in
    try
      let meth = List.assoc name method_table in
      eval_lambda meth.im_body (recv :: args) kwargs
    with Not_found ->
      exc_fail ("Undefined instance method " ^ (inspect_type (type_of_value recv)) ^
                "#" ^ name ^ " for " ^ (inspect_value recv) ^ ".") [loc]

and eval_lambda body args kwargs =
  let lenv = lenv_create (Some body.l_local_env) in
  let tenv = tenv_fork   body.l_type_env  in
  let cenv = ref         body.l_const_env in
  let env  = (lenv, tenv, cenv) in

  let rec bind (kind, name) ~loc ~value ~f_rest ~rest ~kwseen =
    lenv_bind lenv name ~loc ~kind ~value;
    bind_args f_rest rest kwseen

  and bind_args f_args args kwseen =
    match f_args, args with
    | Syntax.FormalSelf((loc,_)) :: f_rest,
      value :: rest
    -> bind (Syntax.LVarImmutable, "self") ~loc ~value ~f_rest ~rest ~kwseen

    | Syntax.FormalArg((loc,_),lvar) :: f_rest,
      value :: rest
    | Syntax.FormalOptArg((loc,_),lvar,_) :: f_rest,
      value :: rest
    -> bind lvar ~loc ~value ~f_rest ~rest ~kwseen

    | Syntax.FormalOptArg((loc,_),lvar,expr) :: f_rest,
      []
    -> bind lvar ~loc ~value:(eval_expr env expr) ~f_rest ~rest:[] ~kwseen

    | Syntax.FormalRest((loc,_),lvar) :: f_rest,
      rest
    -> bind lvar ~loc ~value:(Tuple rest) ~f_rest ~rest:[] ~kwseen

    | Syntax.FormalKwArg((loc,_),((_,name) as lvar)) :: f_rest,
      rest
    -> (let value =
          match Table.get kwargs name with
          | Some v -> v
        in bind lvar ~loc ~value ~f_rest ~rest ~kwseen:(name :: kwseen))

    | Syntax.FormalKwOptArg((loc,_),((_,name) as lvar),expr) :: f_rest,
      rest
    -> (let value =
          match Table.get kwargs name with
          | Some v -> v
          | None   -> eval_expr env expr
        in bind lvar ~loc ~value ~f_rest ~rest ~kwseen:(name :: kwseen))

    | Syntax.FormalKwRest((loc,_),((name,_) as lvar)) :: f_rest,
      rest
    -> (let value  = Record (Table.except_keys kwargs kwseen) in
        let kwseen = Table.keys kwargs in
          bind lvar ~loc ~value ~f_rest ~rest ~kwseen)

    | [], []
    -> (if kwseen <> (Table.keys kwargs) then
          assert false
        else ())
  in
    bind_args body.l_args args [];
    eval env body.l_body

and eval env exprs =
  Option.default Nil
    (List.fold_left (fun _ expr -> Some (eval_expr env expr)) None exprs)
