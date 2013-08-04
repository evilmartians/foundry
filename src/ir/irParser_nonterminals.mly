%{

  open Rt
  open Unicode.Std
  open Big_int

  type named_value =
  | NamedClass     of klass
  | NamedMixin     of mixin
  | NamedPackage   of package
  | NamedLambda    of lambda
  | NamedLocalEnv  of local_env
  | NamedInstance  of klass specialized * slots

  let new_globals () =
    let (kClass, kmetaClass) = Rt.create_class () in
      Table.create [
        (u"c.Class",      NamedClass kClass);
        (u"c.meta:Class", NamedClass kmetaClass);
      ]

  let dummy_local_env =
    { e_parent   = None;
      e_bindings = Table.create [] }

%}

%start <Rt.roots> toplevel

%%
    %public table(X): LBrace
                        xs=separated_list(Comma,
                              separated_pair(Lit_String, Equal, X))
                      RBrace
                      {
                        (fun env ->
                          let xs = List.map (fun (name, x) -> name, x env) xs
                          in Table.create xs)
                      }

      %public seq(X): LBrack
                        xs=separated_list(Comma, X)
                      RBrack
                      { (fun env -> List.map (fun x -> x env) xs) }

%public prefix(X, Y): X y=Y
                      { y }

      location: LParen Lit_Integer Lit_Integer RParen
                { Location.empty }

        global: name=Name_Global
                { (fun env ->
                    match Table.get env name with
                    | Some v -> v
                    | None -> failwith (u"Undefined @" ^ name)) }

     local_env: x=global
                { (fun env ->
                    match x env with
                    | NamedLocalEnv e -> e
                    | _ -> assert false)
                }

        lambda: x=global
                { (fun env ->
                    match x env with
                    | NamedLambda l -> l
                    | _ -> assert false) }

         klass: x=global
                { (fun env ->
                    match x env with
                    | NamedClass k -> k
                    | _ -> assert false) }

         mixin: x=global
                { (fun env ->
                    match x env with
                    | NamedMixin m -> m
                    | _ -> assert false) }

       package: x=global
                { (fun env ->
                    match x env with
                    | NamedPackage p -> p
                    | _ -> assert false) }

      instance: x=global
                { (fun env ->
                    match x env with
                    | NamedInstance (k, sl) -> (k, sl)
                    | _ -> assert false) }

          tvar: Tvar x=Lit_Integer
                { (fun env -> Rt.adopt_tvar (int_of_big_int x)) }

            ty: x=tvar
                { (fun env -> Tvar (x env)) }
              | Type Tvar
                { (fun env -> TvarTy) }
              | Type Nil
                { (fun env -> NilTy) }
              | Type Boolean
                { (fun env -> BooleanTy) }
              | Type Int
                { (fun env -> IntegerTy) }
              | Type Symbol
                { (fun env -> SymbolTy) }
              | Type xs=seq(ty)
                { (fun env -> TupleTy (xs env)) }
              | Type xs=table(ty)
                { (fun env -> RecordTy (xs env)) }
              | Type Environment
                { (fun env -> assert false) }
              | Type Lambda LParen args=ty Comma kwargs=ty RParen Arrow result=ty
                { (fun env -> LambdaTy {
                    l_args_ty   = args   env;
                    l_kwargs_ty = kwargs env;
                    l_result_ty = result env;
                  })
                }

         value: x=ty
                { (fun env -> (x env)) }
              | Nil
                { (fun env -> Nil) }
              | True
                { (fun env -> Truth) }
              | False
                { (fun env -> Lies) }
              | Int x=Lit_Integer
                { (fun env -> Integer x) }
              | Symbol x=Lit_String
                { (fun env -> Symbol x) }
              | xs=seq(value)
                { (fun env -> Tuple (xs env)) }
              | xs=table(value)
                { (fun env -> Record (xs env)) }
              | Environment x=local_env
                { (fun env -> Environment (x env)) }
              | Lambda x=lambda
                { (fun env -> Lambda (x env)) }
              | Class x=klass sp=table(value)
                { (fun env -> Class (x env, sp env)) }
              | Mixin x=mixin sp=table(value)
                { (fun env -> Mixin (x env, sp env)) }
              | Package x=package
                { (fun env -> Package (x env)) }
              | Instance x=instance
                { (fun env -> let (k, sl) = (x env) in Instance (k, sl)) }

     ivar_kind: Immutable
                { Syntax.IVarImmutable }
              | Mutable
                { Syntax.IVarMutable }
              | Meta_mutable
                { Syntax.IVarMetaMutable }

          ivar: loc=location kind=ivar_kind x=value
                { (fun env -> {
                    iv_location = loc;
                    iv_kind     = kind;
                    iv_ty       = x env;
                  }) }

     lvar_kind: Immutable
                { Syntax.LVarImmutable }
              | Mutable
                { Syntax.LVarMutable }

          lvar: loc=location kind=lvar_kind x=value
                { (fun env -> {
                    b_location = loc;
                    b_kind     = kind;
                    b_value    = x env;
                  }) }

       method_: dynamic=boption(Dynamic) x=lambda
                { (fun env -> {
                    im_dynamic = dynamic;
                    im_body    = x env;
                  }) }

        entity: bind_as=Name_Global Equal
                  Class name=Lit_String LBrace
                    metaclass=prefix(Metaclass,           klass)
                     ancestor=prefix(Ancestor,            klass)?
                        tvars=prefix(Type_variables,      table(tvar))?
                        ivars=prefix(Instance_variables,  table(ivar))?
                      methods=prefix(Methods,             table(method_))?
                    prepended=prefix(Prepended,           seq(mixin))?
                     appended=prefix(Appended,            seq(mixin))?
                  RBrace
                { (fun env ->
                    let klass =
                      if bind_as = u"c.Class" or bind_as = u"c.meta:Class" then begin
                        match Table.get_exn env bind_as with
                        | NamedClass k -> k
                        | _ -> assert false
                      end else begin
                        let klass = {
                          k_name      = name;
                          k_ancestor  = Option.map (fun x -> x env) ancestor;
                          k_metaclass = metaclass env;
                          k_tvars     = Table.create [];
                          k_ivars     = Table.create [];
                          k_methods   = Table.create [];
                          k_prepended = [];
                          k_appended  = [];
                        } in
                        Table.set env bind_as (NamedClass klass);
                        klass
                      end
                    in
                    (fun () ->
                      Option.may (fun x -> Table.replace klass.k_tvars   (x env)) tvars;
                      Option.may (fun x -> Table.replace klass.k_ivars   (x env)) ivars;
                      Option.may (fun x -> Table.replace klass.k_methods (x env)) methods;
                      Option.may (fun x -> klass.k_prepended <- (x env)) prepended;
                      Option.may (fun x -> klass.k_appended  <- (x env)) appended))
                }

              | bind_as=Name_Global Equal
                  Mixin name=Lit_String LBrace
                    metaclass=prefix(Metaclass,           klass)
                      methods=prefix(Methods,             table(method_))?
                  RBrace
                { (fun env ->
                    let mixin = {
                      m_name      = name;
                      m_metaclass = metaclass env;
                      m_methods   = Table.create [];
                    } in
                    Table.set env bind_as (NamedMixin mixin);
                    (fun () ->
                      Option.may (fun x -> Table.replace mixin.m_methods (x env)) methods))
                }

              | bind_as=Name_Global Equal
                  Package name=Lit_String LBrace
                    metaclass=prefix(Metaclass,           klass)
                    constants=prefix(Constants,           table(value))?
                  RBrace
                { (fun env ->
                    let package = {
                      p_name      = name;
                      p_metaclass = metaclass env;
                      p_constants = Table.create [];
                    } in
                    Table.set env bind_as (NamedPackage package);
                    (fun () ->
                      Option.may (fun x -> Table.replace package.p_constants (x env)) constants))
                }

              | bind_as=Name_Global Equal
                  Lambda loc=location LBrace
                    local_env=prefix(Local_env,           local_env)
                     type_env=prefix(Type_env,            table(tvar))?
                    const_env=prefix(Const_env,           seq(package))?
                           ty=prefix(Type,                ty)
                         args=Syntax_Args
                         body=Syntax_Exprs
                  RBrace
                { (fun env ->
                    let lambda = {
                      l_location  = loc;
                      l_ty        = ty env;
                      l_local_env = dummy_local_env;
                      l_type_env  = Table.create [];
                      l_const_env = [];
                      l_args      = Syntax.formal_args_of_sexp args;
                      l_body      = Syntax.exprs_of_sexp body;
                    } in
                    Table.set env bind_as (NamedLambda lambda);
                    (fun () ->
                      lambda.l_local_env <- local_env env;
                      Option.may (fun x -> lambda.l_type_env  <- (x env)) type_env;
                      Option.may (fun x -> lambda.l_const_env <- (x env)) const_env))
                }

              | bind_as=Name_Global Equal
                  Environment LBrace
                       parent=prefix(Parent,              local_env)?
                     bindings=prefix(Bindings,            table(lvar))?
                  RBrace
                { (fun env ->
                    let local_env = {
                      e_parent   = Option.map (fun x -> x env) parent;
                      e_bindings = Table.create [];
                    } in
                    Table.set env bind_as (NamedLocalEnv local_env);
                    (fun () ->
                      Option.may (fun x -> Table.replace local_env.e_bindings (x env)) bindings))
                }

              | bind_as=Name_Global Equal
                  Instance klass=klass sp=table(value) slots=table(value)
                { (fun env ->
                    let i_klass = klass env in
                    let i_sp    = sp env in
                    let i_slots = Table.create [] in
                      Table.set env bind_as (NamedInstance ((i_klass, i_sp), i_slots));
                      (fun () ->
                        Table.replace i_slots (slots env)))
                }

   definitions: defs=definitions x=entity
                { let (env, defs) = defs in
                    env, ((x env) :: defs) }
              | /* empty */
                { (new_globals ()), [] }

      toplevel: defs=definitions EOF
                { let (env, defs) = defs in
                    List.iter (fun f -> f ()) defs;
                    let get_class name =
                      match Table.get env name with
                      | Some (NamedClass k) -> k
                      | _ -> assert false
                    in
                    let get_package name =
                      match Table.get env name with
                      | Some (NamedPackage p) -> p
                      | _ -> assert false
                    in
                    {
                      last_tvar     = 0; (* TODO *)

                      kClass        = get_class (u"c.Class");
                      kTypeVariable = get_class (u"c.TypeVariable");
                      kNil          = get_class (u"c.Nil");
                      kBoolean      = get_class (u"c.Boolean");
                      kInteger      = get_class (u"c.Integer");
                      kSymbol       = get_class (u"c.Symbol");
                      kTuple        = get_class (u"c.Tuple");
                      kRecord       = get_class (u"c.Record");
                      kLambda       = get_class (u"c.Lambda");
                      kMixin        = get_class (u"c.Mixin");
                      kPackage      = get_class (u"c.Package");

                      pToplevel     = get_package (u"p.toplevel");
                    }
                }
