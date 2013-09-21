open Sexplib.Std
open Unicode.Std
open ExtList
open Rt

(* Eval helper routines *)

type env = {
          selector  : string option;
          local_env : local_env;
          type_env  : type_env;
  mutable const_env : const_env;
}

let env_create () =
  let env = {
     selector = None;
    local_env = lenv_create None;
     type_env = tenv_create ();
    const_env = cenv_extend cenv_empty !roots.pToplevel
  }
  in
  lenv_bind env.local_env "self" ~value:(Package !roots.pToplevel)
                                 ~kind:Syntax.LVarImmutable
                                 ~loc:Location.empty;
  env

let rec ivars_of_klass klass =
  let ivars = Assoc.keys klass.k_ivars in
  match klass.k_ancestor with
  | Some ancestor -> List.merge compare ivars (ivars_of_klass ancestor)
  | None -> ivars

let lookup_method klass selector =
  let rec lookup' klass' =
    let method_table = klass'.k_methods in
    try
      Some (Assoc.find method_table selector)
    with Not_found ->
      match klass'.k_ancestor with
      | Some ancestor
      -> lookup' ancestor
      | None
      -> None
  in
  lookup' klass

let concat_tuple lhs rhs =
  match lhs, rhs with
  | Tuple(l), Tuple(r)
  -> Tuple(l @ r)
  | _
  -> assert false

let concat_record lhs rhs =
  match lhs, rhs with
  | Record(lft), Record(rgt)
  -> Record(Assoc.merge lft rgt)
  | _
  -> assert false

let define_method obj name body loc =
  match obj with
  | Class (klass,_)
  -> (try
        let meth = Assoc.find klass.k_methods name in
        exc_fail ("Cannot define method " ^ name ^ " on " ^ (inspect_value obj) ^
                  ": it is already defined") [loc; meth.im_body.l_location]
      with Not_found ->
        klass.k_methods <- Assoc.append klass.k_methods name body)
  | value -> exc_type "Class" value [loc]

let define_ivar obj name ty loc =
  match obj with
  | Class (klass,_)
  -> (try
        let ivar = Assoc.find klass.k_ivars name in
        exc_fail ("Cannot define @" ^ name ^ " on " ^
                  (inspect_value obj) ^
                  ": it is already defined with type " ^
                  (inspect_type ivar.iv_ty)) [loc; ivar.iv_location]
      with Not_found ->
        klass.k_ivars <- Assoc.append klass.k_ivars name ty)
  | value -> exc_type "Class" value [loc]

let use_ivar self name loc f =
  let klass = Rt.klass_of_value self in
  if not (List.mem name (ivars_of_klass klass)) then
    exc_fail ("Class " ^ klass.k_name ^ " does not define an instance variable @" ^
              name) [loc];
  match self with
  | Instance(inst) -> f inst
  | _ -> assert false

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
  -> Record (Assoc.sorted [k, eval_expr env v])

  | Syntax.RecordSplice(_,expr)
  -> (match (eval_expr env expr) with
     | Record(_) as r -> r
     | o -> exc_type "Record" o [Syntax.loc expr])

  | Syntax.RecordPair(_,k,v)
  -> (match (eval_expr env k) with
     | Symbol(s) -> Record (Assoc.sorted [s, (eval_expr env v)])
     | o -> exc_type "Symbol" o [Syntax.loc k])

and eval_string env elem =
  match elem with
  | Syntax.QuoteString(_, str)
  -> str
  | Syntax.QuoteSplice((loc, _), expr)
  -> (let value = eval_expr env expr in
      match value with
      | String str
      -> str
      | _
      -> (let value = eval_send value "to_s" ~loc in
          match value with
          | String str -> str
          | _ -> exc_type "string" value [loc]))

and eval_pattern env lhs value =
  match lhs with
  | Syntax.PatVariable((loc,_), (kind, name))
  -> lenv_bind env.local_env name ~kind ~value ~loc
(*
  | Syntax.PatTuple((loc,_), pats)
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
 *)
  | _ -> assert false

and eval_assign env lhs value =
  match lhs with
  | Syntax.Var((loc,_), name)
  -> (lenv_mutate env.local_env name ~value;
      value)
  | Syntax.Const((loc,_), name)
  -> (try
        cenv_bind env.const_env name value;
        value
      with CEnvAlreadyBound value ->
        exc_fail ("Name " ^ name ^ " is already bound to " ^ (inspect value) ^ ".") [loc])
  | Syntax.IVar((loc,_), name)
  -> (let self = lenv_lookup env.local_env "self" in
      use_ivar self name loc (fun inst ->
        let klass, _ = inst.i_class in
        if klass.k_is_value then
          let slots' = Table.copy inst.i_slots in
          Table.set slots' name value;
          lenv_mutate env.local_env "self" (Instance ({
            i_hash  = Hash_seed.make ();
            i_class = inst.i_class;
            i_slots = slots';
          }))
        else
          Table.set inst.i_slots name value);
      value)
  | _
  -> assert false

and eval_type env expr =
  let as_type expr =
    let ty = eval_type env expr in
      match ty with
      | Tvar(_) | TvarTy | NilTy | BooleanTy | IntegerTy | SymbolTy
      | UnsignedTy(_) | SignedTy(_) | TupleTy(_) | RecordTy(_) | LambdaTy(_)
      | Class(_,_)
      -> ty
      | _
      -> exc_type "type" ty [Syntax.ty_loc expr]
  in
  match expr with
  | Syntax.TypeVar(_,name)
  -> Tvar (tenv_resolve env.type_env name)
  | Syntax.TypeTuple(_,xs)
  -> TupleTy (List.map as_type xs)
  | Syntax.TypeRecord(_,xs)
  -> RecordTy (Assoc.sorted (List.map (fun (_,k,v) -> k, as_type v) xs))
  | Syntax.TypeFunction(_, args, ret)
  -> (let lambda_ty_elem_of_arg_ty arg_ty =
        match arg_ty with
        | Syntax.TypeArg(_,ty)      -> LambdaArg   (as_type ty)
        | Syntax.TypeKwArg(_,kw,ty) -> LambdaKwArg (kw, as_type ty)
      in
      LambdaTy (List.map lambda_ty_elem_of_arg_ty args, as_type ret))
  | Syntax.TypeConstr((loc, _), name, args)
  -> (try
        match cenv_lookup env.const_env name with
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
                | Syntax.TypeKwArg(_, name, value)
                -> args, (name, eval_type env value) :: kwargs)
              ([], []) args
            in
            let f_args, kw_f_args =
              List.split_nth (List.length args) (Assoc.keys klass.k_parameters)
            in
            let new_specz = List.combine f_args (List.rev args) in
            let new_specz = List.fold_left (fun acc (kw, ty) ->
                if List.mem_assoc kw new_specz then
                  exc_fail ("Type parameter `" ^ kw ^ "' is passed more than once") [loc]
                else if not (Assoc.mem klass.k_parameters kw) then
                  exc_fail ("Class `" ^ klass.k_name ^ "' is not parametric by `" ^ kw) [loc]
                else
                  (kw, ty) :: acc)
              new_specz kw_args
            in
            let cls = Class (klass, Assoc.merge specz (Assoc.sorted new_specz)) in
            Typing.fold_equiv cls)
        | value
        -> exc_fail ("Name " ^ name ^ " is bound to " ^ (inspect value) ^
                     " which is not a type") [loc]
      with CEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is unbound") [loc])
  | Syntax.TypeSplice(_,expr)
  -> eval_expr env expr

and eval_closure_ty env formal_args ty_expr =
  let lambda_args = Rt.lambda_args_of_formal_args formal_args in
  match ty_expr with
  | Some ty_expr
  -> (let type_env = tenv_fork env.type_env in
      let ty = eval_type { env with type_env } ty_expr in
        match ty with
        | LambdaTy(ty)
        -> type_env, ty, lambda_args
        | _
        -> exc_type "closure type" ty [Syntax.ty_loc ty_expr])
  | None
  -> (let lambda_elem_ty_of_formal_arg arg =
        match arg with
        | Syntax.FormalSelf _
        | Syntax.FormalArg _
        -> LambdaArg (Tvar (new_tvar ()))
        | Syntax.FormalOptArg _
        -> LambdaOptArg (Tvar (new_tvar ()))
        | Syntax.FormalRest _
        -> LambdaRest (Tvar (new_tvar ()))
        | Syntax.FormalKwArg (_, (_, kw))
        -> LambdaKwArg (kw, Tvar (new_tvar ()))
        | Syntax.FormalKwOptArg (_, (_, kw), _)
        -> LambdaKwOptArg (kw, Tvar (new_tvar ()))
        | Syntax.FormalKwRest _
        -> LambdaKwRest (Tvar (new_tvar ()))
      in
      let ty = List.map lambda_elem_ty_of_formal_arg formal_args, Tvar (new_tvar ()) in
      env.type_env, ty, lambda_args)

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
  let rec eval_kwargs args assoc =
    match args with
    | Syntax.ActualKwArg(_,k,v) :: rest
    -> (let assoc = Assoc.add assoc k (eval_expr env v) in
        eval_kwargs rest assoc)

    | Syntax.ActualKwPunArg(_,k) :: rest
    -> (let assoc = Assoc.add assoc k (lenv_lookup env.local_env k) in
        eval_kwargs rest assoc)

    | Syntax.ActualKwSplice(_,expr) :: rest
    -> (match (eval_expr env expr) with
        | Record(r) -> eval_kwargs rest (Assoc.merge assoc r)
        | o         -> exc_type "Record" o [Syntax.loc expr])

    | Syntax.ActualKwPair(_,k,v) :: rest
    -> (match (eval_expr env k) with
        | Symbol(k) -> (let assoc = Assoc.add assoc k (eval_expr env v) in
                        eval_kwargs rest assoc)
        | o         -> exc_type "Symbol" o [Syntax.loc k])

    | _ :: rest -> eval_kwargs rest assoc
    | []        -> assoc
  in
  eval_args lst, eval_kwargs lst Assoc.empty

and eval_expr env expr =
  match expr with
  | Syntax.Nil(_)   -> Nil
  | Syntax.Truth(_) -> Truth
  | Syntax.Lies(_)  -> Lies
  | Syntax.Integer(_,x) -> Integer(x)
  | Syntax.Symbol(_,x) -> Symbol(x)

  | Syntax.Unsigned(_,w,x) -> Unsigned(w,x)
  | Syntax.Signed(_,w,x)   -> Signed(w,x)

  | Syntax.Tuple(_, elems)
  -> List.fold_left concat_tuple
        (Tuple []) (List.map (eval_tuple env) elems)

  | Syntax.Record(_, elems)
  -> List.fold_left concat_record
        (Record Assoc.empty) (List.map (eval_record env) elems)

  | Syntax.Quote(_, quote_as, elems)
  -> (let value =
        List.fold_left (^) "" (List.map (eval_string env) elems)
      in
      match quote_as with
      | Syntax.QuoteAsString -> String value
      | Syntax.QuoteAsSymbol -> Symbol value)

  | Syntax.Type(_, ty_expr)
  -> eval_type { env with type_env = tenv_fork env.type_env } ty_expr

  | Syntax.Const(loc, name)
  -> (try
        cenv_lookup env.const_env name
      with CEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") [Syntax.loc expr])

  | Syntax.Self(loc)
  -> lenv_lookup env.local_env "self"

  | Syntax.Var(loc, name)
  -> lenv_lookup env.local_env name

  | Syntax.IVar((loc, _), name)
  -> (let self = lenv_lookup env.local_env "self" in
      use_ivar self name loc (fun inst ->
        match Table.get inst.i_slots name with
        | Some value -> value
        | None
        -> exc_fail ("Instance variable @" ^ name ^ " is not assigned yet.") [loc]))

  | Syntax.Begin(_, exprs)
  -> eval env exprs

  | Syntax.Let(_, pat, ty, expr)
  -> (let value = eval_expr env expr in
      eval_pattern env pat value;
      value)

  | Syntax.Assign(_, lhs, rhs)
  -> (let value = eval_expr env rhs in
      eval_assign env lhs value)

  | Syntax.OpAssign((loc, _), lhs, meth, rhs)
  -> (let value  = eval_expr env lhs in
      let arg    = eval_expr env rhs in
      let result = eval_send value meth ~args:[arg] ~kwargs:Assoc.empty ~loc in
      eval_assign env lhs result)

  | Syntax.Lambda(_, args, ty_expr, body)
  -> (let tenv, ty, args = eval_closure_ty env args ty_expr in
      Lambda {
        l_hash      = Hash_seed.make ();
        l_location  = Syntax.loc expr;
        l_ty        = ty;
        l_local_env = env.local_env;
        l_type_env  = env.type_env;
        l_const_env = env.const_env;
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
      let cls =
        match cenv_peek env.const_env name with
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
            -> Class (klass_of_value value, Assoc.empty))
        (* Not a class. *)
        | Some value
        -> exc_fail ("Cannot reopen " ^ name ^ ": it is bound to " ^
                     (inspect value) ^ ", which is not a class") [loc]
        (* No class present, create one and inherit specializations from
           its ancestor. *)
        | None
        -> (let eval_param params arg =
              let name, tvar =
                match arg with
                | Syntax.FormalTypeArg (_, name)
                -> name, (tenv_resolve env.type_env name)
                | Syntax.FormalTypeKwArg (_, kw, name)
                -> kw,   (tenv_resolve env.type_env name)
              in
              if Assoc.mem params name then
                Assoc.replace params name tvar
              else
                Assoc.append  params name tvar
            in
            let specz      = Option.default Assoc.empty specz in
            let ancestor   = Option.default !roots.kObject ancestor in
            let parameters = List.fold_left eval_param ancestor.k_parameters params in
            let value      = Class (new_class ~ancestor ~parameters name, specz) in
            cenv_bind env.const_env name value;
            value)
      in
      (* Evaluate class body in a context where self is bound to the class *)
      let local_env = lenv_create (Some env.local_env) in
      lenv_bind local_env "self" ~value:cls ~kind:Syntax.LVarImmutable ~loc;
      eval { env with local_env } body)

  | Syntax.DefMethod((loc,_),name,args,ty_expr,body)
  -> (let definee  =
        match lenv_lookup env.local_env "self" with
        | Package(p) when p == !roots.pToplevel
        -> Class(p.p_metaclass, Assoc.empty)
        | other
        -> other
      in
      let tenv, ty, args = eval_closure_ty env args ty_expr in
      define_method definee name {
        im_hash     = Hash_seed.make ();
        im_dynamic  = false;
        im_body     = {
          l_hash      = Hash_seed.make ();
          l_location  = loc;
          l_ty        = ty;
          l_local_env = env.local_env;
          l_type_env  = env.type_env;
          l_const_env = env.const_env;
          l_args      = args;
          l_body      = body;
        }
      } loc;
      Nil)

  | Syntax.DefSelfMethod((loc,_),name,args,ty_expr,body)
  -> (let tenv, ty, args = eval_closure_ty env args ty_expr in
      let definee  = Class (klass_of_value ~dispatch:true (lenv_lookup env.local_env "self"),
                            Assoc.empty) in
      define_method definee name {
        im_hash     = Hash_seed.make ();
        im_dynamic  = false;
        im_body     = {
          l_hash    = Hash_seed.make ();
          l_location  = loc;
          l_ty        = ty;
          l_local_env = env.local_env;
          l_type_env  = env.type_env;
          l_const_env = env.const_env;
          l_args      = args;
          l_body      = body;
        }
      } loc;
      Nil)

  | Syntax.DefIVar((loc,_),name,kind,ty_expr)
  -> (define_ivar (lenv_lookup env.local_env "self") name {
        iv_hash     = Hash_seed.make ();
        iv_location = loc;
        iv_ty       = eval_type env ty_expr;
        iv_kind     = kind;
      } loc; Nil)

  | Syntax.InvokePrimitive((loc,_), name, args) when name = "lam_call"
  -> (match List.map (eval_expr env) args with
      | [Rt.Lambda lambda; Rt.Tuple args; Rt.Record kwargs]
      -> eval_lambda ~loc lambda args kwargs
      | _
      -> assert false)

  | Syntax.InvokePrimitive(_, name, args)
  -> (let args = List.map (eval_expr env) args in
        Primitive.invoke name args)

  | Syntax.Send((_, { Syntax.selector = loc }), recv, name, args)
  -> (let recv = eval_expr env recv in
      let args, kwargs = eval_args env args in
      eval_send recv name ~args ~kwargs ~loc)

  | Syntax.Super((loc,_), args)
  -> (let self = lenv_lookup env.local_env "self" in
      let args, kwargs = eval_args env args in
      let value = eval_super env self ~args ~kwargs ~loc in
      match self with
      | Instance(inst)
      -> (let klass, _ = inst.i_class in
          if env.selector = Some "initialize" && klass.k_is_value then
            lenv_mutate env.local_env "self" value;
          value)
      | _
      -> value)

  | Syntax.If(_, cond_expr, if_true, if_false)
  -> (let cond = eval_expr env cond_expr in
      match cond with
      | Rt.Truth -> eval env if_true
      | Rt.Lies  -> Option.map_default (eval_expr env) Rt.Nil if_false
      | _ -> exc_type "Boolean" cond [Syntax.loc cond_expr])

  | Syntax.Unless(_, cond_expr, if_false)
  -> (let cond = eval_expr env cond_expr in
      match cond with
      | Rt.Truth -> Rt.Nil
      | Rt.Lies  -> eval env if_false
      | _ -> exc_type "Boolean" cond [Syntax.loc cond_expr])

  | Syntax.While(_, cond_expr, body)
  | Syntax.Until(_, cond_expr, body)
  -> (let rec loop () =
        let cond = eval_expr env cond_expr in
        match cond, expr with
        | Rt.Truth, Syntax.While _
        | Rt.Lies,  Syntax.Until _
        -> ignore (eval env body); loop ()
        | Rt.Lies,  Syntax.While _
        | Rt.Truth, Syntax.Until _
        -> Nil
        | _
        -> exc_type "Boolean" cond [Syntax.loc cond_expr]
      in
      loop ())

  | Syntax.Not (_, expr)
  -> (let value = eval_expr env expr in
      match value with
      | Rt.Truth -> Rt.Lies
      | Rt.Lies  -> Rt.Truth
      | _ -> exc_type "Boolean" value [Syntax.loc expr])

  | Syntax.Or  (_, lhs, rhs) | Syntax.OrAssign  (_, lhs, rhs)
  | Syntax.And (_, lhs, rhs) | Syntax.AndAssign (_, lhs, rhs)
  -> (let value = eval_expr env lhs in
      match expr, value with
      | (Syntax.Or _  | Syntax.OrAssign _),  Rt.Truth
      | (Syntax.And _ | Syntax.AndAssign _), Rt.Lies
      -> value
      | (Syntax.Or _  | Syntax.OrAssign _),  Rt.Lies
      | (Syntax.And _ | Syntax.AndAssign _), Rt.Truth
      -> (let value' = eval_expr env rhs in
          match expr with
          | Syntax.OrAssign _ | Syntax.AndAssign _
          -> eval_assign env lhs value'
          | _ -> value')
      | _ -> exc_type "Boolean" value [Syntax.loc lhs])

  | Syntax.TVar (_, _) | Syntax.Update (_, _)
  -> failwith ("cannot eval " ^
               (Unicode.assert_utf8s
                (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr))));

and eval_send ?(args=[]) ?(kwargs=Assoc.empty) ~loc recv selector =
  let klass = klass_of_value ~dispatch:true recv in
  let meth  =
    match lookup_method klass selector with
    | Some meth
    -> meth
    | None
    -> exc_fail ("Undefined instance method " ^ klass.k_name ^
                 "#" ^ selector ^ " for " ^ (inspect_value recv) ^ ".") [loc]
  in
  let result = eval_lambda ~loc ~selector meth.im_body (recv :: args) kwargs in
  if selector = "initialize" then begin
    match result with
    | Instance({ i_class = klass, specz; i_slots = slots; })
    -> (let slot_ivars    = List.sort (Table.keys slots)
        and defined_ivars = ivars_of_klass klass in
        let diff_ivars    = List.fold_left List.remove defined_ivars slot_ivars in
        let diff_ivars    = String.concat ", " (List.map (fun iv -> "@" ^ iv) diff_ivars) in
        if slot_ivars <> defined_ivars then
          exc_fail ("Initializer did not initialize slots: " ^ diff_ivars ^ ".")
                   [meth.im_body.l_location];
        result)
    | _
    -> assert false
  end else result

and eval_super env ~args ~kwargs ~loc recv =
  let klass = klass_of_value ~dispatch:true recv in
  let klass =
    match klass.k_ancestor with
    | Some klass -> klass
    | None
    -> exc_fail ("Class " ^ klass.k_name ^ " does not have a superclass.") [loc]
  and selector =
    match env.selector with
    | Some selector -> selector
    | None
    -> exc_fail ("Cannot call superclass method from a non-method closure.") [loc]
  in
  let meth  =
    match lookup_method klass selector with
    | Some meth -> meth
    | None
    -> exc_fail ("Undefined superclass method " ^ klass.k_name ^
                 "#" ^ selector ^ " for " ^ (inspect_value recv) ^ ".") [loc]
  in
  eval_lambda ~loc ~selector meth.im_body (recv :: args) kwargs

and eval_lambda ~loc ?selector body args kwargs =
  let env = {
    selector  = selector;
    local_env = lenv_create (Some body.l_local_env);
     type_env = tenv_fork body.l_type_env;
    const_env = body.l_const_env
  }
  in
  let rec bind ~f_arg ~ty ~value ~f_args ~f_arg_tys ~rest ~kwseen =
    (* TODO check ty *)
    let kind =
      match (f_arg.la_name :> latin1s), value with
      | "self", Instance ({ i_class = klass, _; }) when klass.k_is_value
      -> Syntax.LVarMutable
      | _
      -> f_arg.la_kind
    in
    lenv_bind env.local_env f_arg.la_name ~loc:f_arg.la_location ~kind ~value;
    bind_args f_args f_arg_tys rest kwseen

  and bind_args f_args f_arg_tys rest kwseen =
    match f_args, f_arg_tys, rest with
    | [], [], []
    -> (
        let unexpected = Assoc.filter_map_list (fun key _ ->
            if List.mem key kwseen then None
            else Some key)
          kwargs
        in
        if unexpected = [] then
          ()
        else
          exc_fail ("Unexpected keyword arguments " ^ (String.concat ", " unexpected) ^ ".") [loc])

    | [], [], _
    -> (let expected = List.length body.l_args - 1
        and actual   = List.length args in
        exc_fail ("Too much positional arguments: " ^ (string_of_int expected) ^
                  " expected, " ^ (string_of_int actual) ^ " provided.")) [loc]

    | f_arg :: f_args, LambdaArg ty    :: f_arg_tys, value :: rest
    | f_arg :: f_args, LambdaOptArg ty :: f_arg_tys, value :: rest
    -> bind ~f_arg ~ty ~value ~f_args ~f_arg_tys ~rest ~kwseen

    | f_arg :: f_args, LambdaArg ty    :: f_arg_tys, []
    -> (let num = List.length body.l_args - List.length f_args in
        exc_fail ("Positional argument " ^ (string_of_int num) ^
                  " is expected but not provided.")) [loc]

    | f_arg :: f_args, LambdaOptArg ty :: f_arg_tys, []
    -> (let value = eval_expr env (Option.get f_arg.la_default) in
        bind ~f_arg ~ty ~value ~f_args ~f_arg_tys ~rest:[] ~kwseen)

    | f_arg :: f_args, LambdaRest ty :: f_arg_tys, rest
    -> bind ~f_arg ~ty ~value:(Tuple rest) ~f_args ~f_arg_tys ~rest:[] ~kwseen

    | f_arg :: f_args, LambdaKwArg (kw, ty) :: f_arg_tys, rest
    -> (let value =
          match Assoc.find_option kwargs kw with
          | Some v -> v
          | None -> exc_fail ("Keyword argument " ^ kw ^ " is expected but not provided.") [loc]
        in
        bind ~f_arg ~ty ~value ~f_args ~f_arg_tys ~rest ~kwseen:(kw :: kwseen))

    | f_arg :: f_args, LambdaKwOptArg (kw, ty) :: f_arg_tys, rest
    -> (let kwseen, value =
          match Assoc.find_option kwargs kw with
          | Some v -> kw :: kwseen, v
          | None   -> kwseen, eval_expr env (Option.get f_arg.la_default)
        in
        bind ~f_arg ~ty ~value ~f_args ~f_arg_tys ~rest ~kwseen:(kw :: kwseen))

    | f_arg :: f_args, LambdaKwRest ty :: f_arg_tys, rest
    -> (let value  = Record (Assoc.filter kwargs ~f:(fun kw _ -> not (List.mem kw kwseen))) in
        bind ~f_arg ~ty ~value ~f_args ~f_arg_tys ~rest ~kwseen:(Assoc.keys kwargs))

    | [], _::_, _
    | _::_, [], _
    -> assert false
  in
  bind_args body.l_args (fst body.l_ty) args [];

  let result = eval env body.l_body in
  if selector = Some "initialize" then
    lenv_lookup env.local_env "self"
  else
    result

and eval env exprs =
  Option.default Nil
    (List.fold_left (fun _ expr -> Some (eval_expr env expr)) None exprs)
