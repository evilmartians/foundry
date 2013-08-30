open Sexplib.Std
open Unicode.Std
open ExtList
open Rt

let rec klass_ivars klass =
  let ivars = Assoc.keys klass.k_ivars in
  match klass.k_ancestor with
  | Some ancestor -> ivars @ klass_ivars ancestor
  | None -> ivars

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

let define_ivar obj name body loc =
  match obj with
  | Class (klass,_)
  -> (try
        let ivar = Assoc.find klass.k_ivars name in
        exc_fail ("Cannot define @" ^ name ^ " on " ^
                  (inspect_value obj) ^
                  ": it is already defined with type " ^
                  (inspect_type ivar.iv_ty)) [loc; ivar.iv_location]
      with Not_found ->
        klass.k_ivars <- Assoc.append klass.k_ivars name body)
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
  -> Record (Assoc.sorted [k, (eval_expr env v)])

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
  | Syntax.Var((loc,_), name)
  -> lenv_mutate lenv name ~value
  | Syntax.Const((loc,_), name)
  -> (try
        cenv_bind !cenv name value
      with CEnvAlreadyBound value ->
        exc_fail ("Name " ^ name ^ " is already bound to " ^ (inspect value) ^ ".") [loc])
  | Syntax.IVar((loc,_), name)
  -> (let self  = lenv_lookup lenv "self" in
      let klass = Rt.klass_of_value self  in
      if not (List.mem name (klass_ivars klass)) then
        exc_fail ("Class " ^ klass.k_name ^ " does not define an instance variable @" ^
                  name) [loc];
      match self with
      | Instance(inst)
      -> (if (fst inst.i_class).k_is_value then
            let slots' = Table.copy inst.i_slots in
            Table.set slots' name value;
            lenv_mutate lenv "self" (Instance ({
              i_hash  = Hash_seed.make ();
              i_class = inst.i_class;
              i_slots = slots';
            }))
          else
            Table.set inst.i_slots name value)
      | _
      -> assert false)
  | _
  -> assert false

and eval_type ((lenv, tenv, cenv) as env) expr =
  let as_type expr =
    let ty = eval_type env expr in
      match ty with
      | Tvar(_) | TvarTy | NilTy | BooleanTy
      | IntegerTy | SymbolTy
      | UnsignedTy(_) | SignedTy(_)
      | TupleTy(_) | RecordTy(_) | LambdaTy(_)
      | Class(_,_)
      -> ty
      | _
      -> exc_type "type" ty [Syntax.ty_loc expr]
  in
  match expr with
  | Syntax.TypeVar(_,name)
  -> Tvar (tenv_resolve tenv name)
  | Syntax.TypeTuple(_,xs)
  -> TupleTy (List.map as_type xs)
  | Syntax.TypeRecord(_,xs)
  -> RecordTy (Assoc.sorted (List.map (fun (_,k,v) -> k, as_type v) xs))
  | Syntax.TypeFunction(_, args, ret)
  -> (let args, kwargs =
        List.fold_left (fun (args, kwargs) arg ->
          match arg with
          | Syntax.TypeArg(_,ty)     -> ((as_type ty) :: args), kwargs
          | Syntax.TypeKwArg(_,n,ty) -> args, (n, as_type ty) :: kwargs)
        ([], []) args
      in LambdaTy {
        l_ty_args   = TupleTy  (List.rev args);
        l_ty_kwargs = RecordTy (Assoc.sorted kwargs);
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
                  (kw, ty) :: acc) new_specz kw_args
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
  let rec eval_kwargs args assoc =
    match args with
    | Syntax.ActualKwArg(_,k,v) :: rest
    -> (let assoc = Assoc.add assoc k (eval_expr env v) in
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

and eval_expr ((lenv, tenv, cenv) as env) expr =
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
      let result = eval_send value meth ~args:[arg] ~kwargs:Assoc.empty ~loc in
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
      let cls =
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
                -> name, (tenv_resolve tenv name)
                | Syntax.FormalTypeKwArg (_, kw, name)
                -> kw,   (tenv_resolve tenv name)
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
            cenv_bind !cenv name value;
            value)
      in
      (* Evaluate class body in a context where self is bound to the class *)
      let lenv = lenv_create (Some lenv) in
      lenv_bind lenv "self" ~value:cls ~kind:Syntax.LVarImmutable ~loc;
      eval (lenv, tenv, cenv) body)

  | Syntax.DefMethod((loc,_),name,args,ty_expr,body)
  -> (let definee  =
        match lenv_lookup lenv "self" with
        | Package(p) when p == !roots.pToplevel
        -> Class(p.p_metaclass, Assoc.empty)
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
                            Assoc.empty) in
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

  | Syntax.Send((_, { Syntax.selector = loc }), recv, name, args)
  -> (let recv = eval_expr env recv in
      let args, kwargs = eval_args env args in
      eval_send recv name ~args ~kwargs ~loc)

  | Syntax.If(_, cond_expr, if_true, if_false)
  -> (let cond = eval_expr env cond_expr in
      match cond with
      | Rt.Truth -> eval env if_true
      | Rt.Lies  -> Option.map_default (eval_expr env) Rt.Nil if_false
      | _ -> exc_type "boolean value" cond [Syntax.loc cond_expr])

  | Syntax.Or (_, lhs_expr, rhs_expr)
  -> (let lhs = eval_expr env lhs_expr in
      match lhs with
      | Rt.Truth -> lhs
      | Rt.Lies  -> eval_expr env rhs_expr
      | _ -> exc_type "boolean value" lhs [Syntax.loc lhs_expr])

  | Syntax.And (_, lhs_expr, rhs_expr)
  -> (let lhs = eval_expr env lhs_expr in
      match lhs with
      | Rt.Truth -> eval_expr env rhs_expr
      | Rt.Lies  -> lhs
      | _ -> exc_type "boolean value" lhs [Syntax.loc lhs_expr])

  | _
  -> failwith ("cannot eval " ^
               (Unicode.assert_utf8s
                (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr))));

and eval_send ?(args=[]) ?(kwargs=Assoc.empty) recv selector ~loc =
  let rec lookup klass =
    let method_table = klass.k_methods in
    try
      Assoc.find method_table selector
    with Not_found ->
      match klass.k_ancestor with
      | Some ancestor
      -> lookup ancestor
      | None
      -> (let klass = klass_of_value ~dispatch:true recv in
          exc_fail ("Undefined instance method " ^ klass.k_name ^
                    "#" ^ selector ^ " for " ^ (inspect_value recv) ^ ".") [loc])
  in
  let klass  = klass_of_value ~dispatch:true recv in
  let result = eval_lambda (lookup klass).im_body (recv :: args) kwargs in
  if selector = "initialize" then begin
    match recv with
    | Instance(inst)
    -> (let klass, _      = inst.i_class in
        let slot_ivars    = List.sort (Table.keys inst.i_slots)
        and defined_ivars = klass_ivars klass in
        let diff_ivars    = List.fold_left List.remove defined_ivars slot_ivars in
        let diff_ivars    = String.concat ", " (List.map (fun iv -> "@" ^ iv) diff_ivars) in
        if slot_ivars <> defined_ivars then
          exc_fail ("Initializer did not initialize slots: " ^ diff_ivars) [loc];
        recv)
    | _
    -> assert false
  end else result

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
    | Syntax.FormalSelf((loc,_)) :: f_rest, value :: rest
    -> (let kind =
          match value with
          | Instance ({ i_class = klass, _; }) when klass.k_is_value
          -> Syntax.LVarMutable
          | _
          -> Syntax.LVarImmutable
        in
        bind (kind, "self") ~loc ~value ~f_rest ~rest ~kwseen)

    | Syntax.FormalArg((loc,_),lvar) :: f_rest, value :: rest
    | Syntax.FormalOptArg((loc,_),lvar,_) :: f_rest, value :: rest
    -> bind lvar ~loc ~value ~f_rest ~rest ~kwseen

    | Syntax.FormalOptArg((loc,_),lvar,expr) :: f_rest, []
    -> bind lvar ~loc ~value:(eval_expr env expr) ~f_rest ~rest:[] ~kwseen

    | Syntax.FormalRest((loc,_),lvar) :: f_rest, rest
    -> bind lvar ~loc ~value:(Tuple rest) ~f_rest ~rest:[] ~kwseen

    | Syntax.FormalKwArg((loc,_),((_,name) as lvar)) :: f_rest, rest
    -> (let value =
          match Assoc.find_option kwargs name with
          | Some v -> v
          | None -> assert false
        in
        bind lvar ~loc ~value ~f_rest ~rest ~kwseen:(name :: kwseen))

    | Syntax.FormalKwOptArg((loc,_),((_,name) as lvar),expr) :: f_rest, rest
    -> (let value =
          match Assoc.find_option kwargs name with
          | Some v -> v
          | None   -> eval_expr env expr
        in
        bind lvar ~loc ~value ~f_rest ~rest ~kwseen:(name :: kwseen))

    | Syntax.FormalKwRest((loc,_),((name,_) as lvar)) :: f_rest, rest
    -> (let value  = Record (Assoc.filter kwargs ~f:(fun kw _ -> List.mem kw kwseen)) in
        let kwseen = Assoc.keys kwargs in
        bind lvar ~loc ~value ~f_rest ~rest ~kwseen)

    | [], []
    -> (if kwseen <> (Assoc.keys kwargs) then
          assert false
        else ())

    | _, _
    -> assert false
  in
  bind_args body.l_args args [];
  eval env body.l_body

and eval env exprs =
  Option.default Nil
    (List.fold_left (fun _ expr -> Some (eval_expr env expr)) None exprs)
