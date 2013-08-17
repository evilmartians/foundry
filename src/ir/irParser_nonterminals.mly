%{

  open Unicode.Std
  open Big_int
  open Rt
  open Ssa

  type named_value =
  | NamedClass     of klass
  | NamedMixin     of mixin
  | NamedPackage   of package
  | NamedLambda    of lambda
  | NamedLocalEnv  of local_env
  | NamedInstance  of (klass specialized * slots)
  | NamedFunction  of name

  let create_globals () =
    let (kClass, kmetaClass) = Rt.create_class () in
      Table.create [
        (u"c.Class",      NamedClass kClass);
        (u"c.meta:Class", NamedClass kmetaClass);
      ]

  let dummy_local_env =
    { e_parent   = None;
      e_bindings = Table.create [] }

%}

%start <Rt.roots * Ssa.capsule> toplevel

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

     %public args(X): LParen
                        xs=separated_list(Comma, X)
                      RParen
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

          func: x=global
                { (fun env ->
                    match x env with
                    | NamedFunction f -> f
                    | _ -> assert false) }

          tvar: Tvar LParen x=Lit_Integer RParen
                { (fun env -> Rt.adopt_tvar (int_of_big_int x)) }

     lambda_ty: Lambda LParen args=ty Comma kwargs=ty RParen Arrow result=ty
                { (fun env -> {
                    l_args_ty   = args   env;
                    l_kwargs_ty = kwargs env;
                    l_result_ty = result env;
                  })
                }

environment_ty: Arrow xs=table(lvar_ty) parent=environment_ty
                { (fun env -> Some {
                    e_parent_ty   = parent env;
                    e_bindings_ty = xs env;
                  }) }
              | /* nothing */
                { (fun env -> None) }

      ty_const: Nil
                { (fun env -> NilTy) }
              | Boolean
                { (fun env -> BooleanTy) }
              | Int
                { (fun env -> IntegerTy) }
              | Unsigned LParen w=Lit_Integer RParen
                { (fun env -> UnsignedTy (int_of_big_int w)) }
              | Signed LParen w=Lit_Integer RParen
                { (fun env -> SignedTy (int_of_big_int w)) }
              | Symbol
                { (fun env -> SymbolTy) }
              | xs=seq(ty)
                { (fun env -> TupleTy (xs env)) }
              | xs=table(ty)
                { (fun env -> RecordTy (xs env)) }
              | Environment xs=table(lvar_ty) parent=environment_ty
                { (fun env -> EnvironmentTy {
                    e_parent_ty   = parent env;
                    e_bindings_ty = xs env;
                  }) }
              | x=lambda_ty
                { (fun env -> LambdaTy (x env)) }
              | args=args(ty) Arrow ty=ty
                { (fun env -> FunctionTy (args env, ty env)) }

            ty: x=tvar
                { (fun env -> Tvar (x env)) }
              | Type Tvar
                { (fun env -> TvarTy) }
              | x=ty_const
                { x }

         value: x=tvar
                { (fun env -> Tvar (x env)) }
              | Type Tvar
                { (fun env -> TvarTy) }
              | Type x=ty_const
                { (fun env -> (x env)) }
              | Nil
                { (fun env -> Nil) }
              | True
                { (fun env -> Truth) }
              | False
                { (fun env -> Lies) }
              | Int x=Lit_Integer
                { (fun env -> Integer x) }
              | Unsigned LParen w=Lit_Integer RParen x=Lit_Integer
                { (fun env -> Unsigned (int_of_big_int w, x)) }
              | Signed LParen w=Lit_Integer RParen x=Lit_Integer
                { (fun env -> Signed (int_of_big_int w, x)) }
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

       lvar_ty: loc=location kind=lvar_kind x=ty
                { (fun env -> {
                    b_location_ty = loc;
                    b_kind_ty     = kind;
                    b_value_ty    = x env;
                  }) }

       method_: dynamic=boption(Dynamic) x=lambda
                { (fun env -> {
                    im_dynamic = dynamic;
                    im_body    = x env;
                  }) }

   struct_body: bind_as=Name_Global Equal
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
                      if bind_as = u"c.Class" || bind_as = u"c.meta:Class" then begin
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
                           ty=prefix(Type,                lambda_ty)
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


       fun_arg: ty=ty name=Name_Local
                { (fun env -> ty env, name) }

     func_body: bind_as=Name_Global Equal
                  Function args=args(fun_arg) Arrow result=ty LBrace
                    blocks=basic_blocks
                  RBrace
                {
                  (fun venv ->
                    let fenv  = Table.create [] in
                    let args  = args venv in
                    let funcn = create_func ~id:bind_as
                                  ~arg_ids:(List.map snd args)
                                  (List.map fst args) (result venv) in
                      Table.set venv bind_as (NamedFunction funcn);
                      List.iter (fun arg -> Table.set fenv arg.id arg)
                        (func_of_name funcn).arguments;
                      let fixups = blocks (venv, funcn, fenv) in
                      funcn, (fun () -> List.iter (fun f -> f ()) fixups))
                }

   basic_block: id=Name_Label instrs=nonempty_list(instr)
                { (fun ((venv, func, fenv) as env) ->
                    let basic_block = create_block ~id func in
                      Table.set fenv id basic_block;
                      List.map (fun instr -> instr (venv, basic_block, fenv))
                        instrs) }

  basic_blocks: xs=nonempty_list(basic_block)
                { (fun env ->
                    List.concat (List.map (fun x -> x env) xs)) }

       operand: id=Name_Local
                { (fun (venv, block, fenv) ->
                    Table.get_exn fenv id) }
              | value=value
                { (fun (venv, block, fenv) ->
                    name_of_value (value venv)) }

  opt_local_eq: id=Name_Local Equal
                { id }
              | /* nothing */
                { u"" }

         instr: id=opt_local_eq x=value_instr
                { (fun ((venv, block, fenv) as env) ->
                    let instrn = create_instr ~id Rt.NilTy InvalidInstr in
                    append_instr instrn block;
                    if Table.exists fenv id && id <> u"" then
                      failwith (u"Duplicate name %" ^ id);
                    Table.set fenv instrn.id instrn;
                    (fun () ->
                      let ty, opcode = x env in
                      replace_instr instrn ~ty ~opcode)) }
              | x=term_instr
                { (fun ((venv, blockn, fenv) as env) ->
                    let instrn = create_instr Rt.NilTy InvalidInstr in
                    append_instr instrn blockn;
                    (fun () ->
                      replace_instr ~ty:Rt.NilTy ~opcode:(x env) instrn)) }

      instr_ty: ty=ty
                { (fun (venv, block, fenv) -> ty venv) }

   phi_operand: LBrack name=Name_Local FatArrow operand=operand RBrack
                { name, operand }

  phi_operands: xs=separated_nonempty_list(Comma, phi_operand)
                { (fun ((venv, block, fenv) as env) ->
                    List.map (fun (id, value) ->
                      Table.get_exn fenv id, value env) xs) }

     call_func: id=Name_Local
                { (fun (venv, block, fenv) ->
                    Table.get_exn fenv id) }
              | func=func
                { (fun (venv, block, fenv) ->
                    func venv) }

   value_instr: ty=instr_ty Phi operands=phi_operands
                { (fun env -> ty env, PhiInstr (operands env)) }
              | ty=instr_ty Frame parent=operand
                { (fun env -> ty env, FrameInstr (parent env)) }
              | ty=instr_ty Frame Empty
                { (fun env ->
                    ty env,
                    FrameInstr (name_of_value (Environment {
                                e_parent   = None;
                                e_bindings = Table.create [];
                              }))) }
              | ty=instr_ty Lvar_load
                    lenv=operand Comma name=Lit_String
                { (fun env -> ty env, LVarLoadInstr (lenv env, name)) }
              | ty=instr_ty Lvar_store
                    lenv=operand Comma name=Lit_String Comma value=operand
                { (fun env -> ty env, LVarStoreInstr (lenv env, name, value env)) }
              | ty=instr_ty? Primitive name=Lit_String args=args(operand)
                { (fun env ->
                    Option.map_default (fun ty -> ty env) Rt.NilTy ty,
                    (PrimitiveInstr (name, args env))) }
              | ty=instr_ty? Call func=call_func args=args(operand)
                { (fun env ->
                    Option.map_default (fun ty -> ty env) Rt.NilTy ty,
                    (CallInstr (func env, args env))) }

    term_instr: Jump target=operand
                { (fun env -> JumpInstr (target env)) }
              | Jump_if cond=operand Comma if_true=operand Comma if_false=operand
                { (fun env -> JumpIfInstr (cond env, if_true env, if_false env)) }
              | Return value=operand
                { (fun env -> ReturnInstr (value env) ) }

   definitions: env=definitions x=struct_body
                { let (globals, capsule, fixups) = env in
                  globals, capsule, (x globals) :: fixups }
              | env=definitions x=func_body
                { let (globals, capsule, fixups) = env in
                  let funcn, fixup = x globals in
                  add_func funcn capsule;
                  globals, capsule, fixup :: fixups }
              | /* empty */
                { create_globals (), create_capsule (), [] }

      toplevel: env=definitions EOF
                { let (globals, capsule, fixups) = env in
                    List.iter (fun f -> f ()) fixups;
                    let get_class name =
                      match Table.get globals name with
                      | Some (NamedClass k) -> k
                      | _ -> assert false
                    in
                    let get_package name =
                      match Table.get globals name with
                      | Some (NamedPackage p) -> p
                      | _ -> assert false
                    in
                    let get_func name =
                      match Table.get globals name with
                      | Some (NamedFunction f) -> f
                      | _ -> assert false
                    in
                    (*let roots = {
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
                    in*)
                    create_roots (), capsule
                }
