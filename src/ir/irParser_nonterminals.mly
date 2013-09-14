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
  | NamedInstance  of instance
  | NamedFunction  of name

  let create_globals () =
    let roots = Rt.create_roots () in
      Table.create [
        (u"c.Class",              NamedClass roots.kClass);
        (u"c.meta:Class",         NamedClass roots.kClass.k_metaclass);
        (u"c.Object",             NamedClass roots.kObject);
        (u"c.meta:Object",        NamedClass roots.kObject.k_metaclass);
        (u"c.Value",              NamedClass roots.kValue);
        (u"c.meta:Value",         NamedClass roots.kValue.k_metaclass);
        (u"c.TypeVariable",       NamedClass roots.kTypeVariable);
        (u"c.meta:TypeVariable",  NamedClass roots.kTypeVariable.k_metaclass);
        (u"c.Nil",                NamedClass roots.kNil);
        (u"c.meta:Nil",           NamedClass roots.kNil.k_metaclass);
        (u"c.Boolean",            NamedClass roots.kBoolean);
        (u"c.meta:Boolean",       NamedClass roots.kBoolean.k_metaclass);
        (u"c.Integer",            NamedClass roots.kInteger);
        (u"c.meta:Integer",       NamedClass roots.kInteger.k_metaclass);
        (u"c.Fixed",              NamedClass roots.kFixed);
        (u"c.meta:Fixed",         NamedClass roots.kFixed.k_metaclass);
        (u"c.Symbol",             NamedClass roots.kSymbol);
        (u"c.meta:Symbol",        NamedClass roots.kSymbol.k_metaclass);
        (u"c.String",             NamedClass roots.kString);
        (u"c.meta:String",        NamedClass roots.kString.k_metaclass);
        (u"c.Option",             NamedClass roots.kOption);
        (u"c.meta:Option",        NamedClass roots.kOption.k_metaclass);
        (u"c.Tuple",              NamedClass roots.kTuple);
        (u"c.meta:Tuple",         NamedClass roots.kTuple.k_metaclass);
        (u"c.Record",             NamedClass roots.kRecord);
        (u"c.meta:Record",        NamedClass roots.kRecord.k_metaclass);
        (u"c.Lambda",             NamedClass roots.kLambda);
        (u"c.meta:Lambda",        NamedClass roots.kLambda.k_metaclass);
        (u"c.Mixin",              NamedClass roots.kMixin);
        (u"c.meta:Mixin",         NamedClass roots.kMixin.k_metaclass);
        (u"c.Package",            NamedClass roots.kPackage);
        (u"c.meta:Package",       NamedClass roots.kPackage.k_metaclass);
        (u"p.toplevel",           NamedPackage roots.pToplevel);
        (u"p.meta:toplevel",      NamedClass roots.pToplevel.p_metaclass);
      ]

  let extract_roots last_tvar globals =
    let get_class name =
      match Table.get globals name with
      | Some (NamedClass k) -> k
      | _ -> assert false
    and get_package name =
      match Table.get globals name with
      | Some (NamedPackage p) -> p
      | _ -> assert false
    in
    {
      last_tvar;

      kClass        = get_class (u"c.Class");
      kObject       = get_class (u"c.Object");
      kValue        = get_class (u"c.Value");
      kTypeVariable = get_class (u"c.TypeVariable");
      kNil          = get_class (u"c.Nil");
      kBoolean      = get_class (u"c.Boolean");
      kInteger      = get_class (u"c.Integer");
      kFixed        = get_class (u"c.Fixed");
      kSymbol       = get_class (u"c.Symbol");
      kString       = get_class (u"c.String");
      kOption       = get_class (u"c.Option");
      kTuple        = get_class (u"c.Tuple");
      kRecord       = get_class (u"c.Record");
      kLambda       = get_class (u"c.Lambda");
      kMixin        = get_class (u"c.Mixin");
      kPackage      = get_class (u"c.Package");

      pToplevel     = get_package (u"p.toplevel");
    }

  let dummy_lambda_ty = [], Rt.NilTy

  let dummy_local_env =
    { e_hash     = Hash_seed.make ();
      e_parent   = None;
      e_bindings = Table.create [] }

  type definitions = {
            globals   : named_value Table.t;
    mutable g_fixups  : (unit -> unit) list;
            capsule   : capsule;
    mutable c_fixups  : (unit -> unit) list;
    mutable last_tvar : int;
  }

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

  %public assoc_seq(X): LBrace
                          xs=separated_list(Comma,
                                separated_pair(Lit_String, Equal, X))
                        RBrace
                        { (fun env -> Assoc.sequental (List.map (fun (name, x) -> name, x env) xs)) }

  %public assoc_ord(X): xs=assoc_seq(X)
                        { (fun env -> Assoc.sort (xs env)) }

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

      location: LParen lft=Lit_Integer rgt=Lit_Integer RParen
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
              | Empty
                { (fun env -> dummy_local_env) }

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
                    | NamedInstance i -> i
                    | _ -> assert false) }

          func: x=global
                { (fun env ->
                    match x env with
                    | NamedFunction f -> f
                    | _ -> assert false) }

          tvar: Tvar LParen x=Lit_Integer RParen
                { (fun env -> Rt.adopt_tvar (int_of_big_int x)) }

lambda_ty_elem: ty=ty
                { (fun env -> LambdaArg (ty env)) }
              | Question ty=ty
                { (fun env -> LambdaOptArg (ty env)) }
              | Star ty=ty
                { (fun env -> LambdaRest (ty env)) }
              | kw=Lit_String Equal ty=ty
                { (fun env -> LambdaKwArg (kw, ty env)) }
              | Question kw=Lit_String Equal ty=ty
                { (fun env -> LambdaKwOptArg (kw, ty env)) }
              | StarStar ty=ty
                { (fun env -> LambdaKwRest (ty env)) }

     lambda_ty: Lambda args=args(lambda_ty_elem) Arrow result=ty
                { (fun env -> args env, result env) }

environment_ty: Arrow xs=table(lvar_ty) parent=environment_ty
                { (fun env -> Some {
                    e_ty_parent   = parent env;
                    e_ty_bindings = xs env;
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
              | String
                { (fun env -> StringTy) }
              | Option x=ty
                { (fun env -> OptionTy (x env)) }
              | xs=seq(ty)
                { (fun env -> TupleTy (xs env)) }
              | xs=assoc_ord(ty)
                { (fun env -> RecordTy (xs env)) }
              | Environment xs=table(lvar_ty) parent=environment_ty
                { (fun env -> EnvironmentTy {
                    e_ty_parent   = parent env;
                    e_ty_bindings = xs env;
                  }) }
              | Class x=klass sp=assoc_ord(value)
                { (fun env -> Class (x env, sp env)) }
              | x=lambda_ty
                { (fun env -> LambdaTy (x env)) }
              | Function args=args(ty) Arrow ty=ty
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
              | String x=Lit_String
                { (fun env -> String x) }
              | Option LParen x=value? RParen
                { (fun env -> Option (Option.map (fun x -> x env) x)) }
              | xs=seq(value)
                { (fun env -> Tuple (xs env)) }
              | xs=assoc_ord(value)
                { (fun env -> Record (xs env)) }
              | Environment x=local_env
                { (fun env -> Environment (x env)) }
              | Lambda x=lambda
                { (fun env -> Lambda (x env)) }
              | Class x=klass sp=assoc_ord(value)
                { (fun env -> Class (x env, sp env)) }
              | Mixin x=mixin sp=assoc_ord(value)
                { (fun env -> Mixin (x env, sp env)) }
              | Package x=package
                { (fun env -> Package (x env)) }
              | Instance x=instance
                { (fun env -> Instance (x env)) }

     ivar_kind: Immutable
                { Syntax.IVarImmutable }
              | Mutable
                { Syntax.IVarMutable }
              | Meta_mutable
                { Syntax.IVarMetaMutable }

          ivar: loc=location kind=ivar_kind x=ty
                { (fun env -> {
                    iv_hash     = Hash_seed.make ();
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
                    b_ty_location = loc;
                    b_ty_kind     = kind;
                    b_ty    = x env;
                  }) }

       method_: dynamic=boption(Dynamic) x=lambda
                { (fun env -> {
                    im_hash    = Hash_seed.make ();
                    im_dynamic = dynamic;
                    im_body    = x env;
                  }) }

    lambda_arg: loc=location kind=lvar_kind name=Lit_String
                { {
                    la_location = loc;
                    la_kind     = kind;
                    la_name     = name;
                  } }

   lambda_args: Args LBrack args=separated_list(Comma, lambda_arg) RBrack
                { args }

   lambda_code: args=lambda_args body=Syntax_Exprs
                { args, body }
              | lam=Syntax_Lambda
                { match lam with
                  | Syntax.Lambda (_, args, _, body)
                  -> Rt.lambda_args_of_formal_args args, [body]
                  | _
                  -> assert false }

   struct_body: bind_as=Name_Global Equal
                  Class name=Lit_String LBrace
                    metaclass=prefix(Metaclass,   klass)
                  objectclass=prefix(Objectclass, klass)?
                     ancestor=prefix(Ancestor,    klass)?
                   parameters=prefix(Parameters,  assoc_seq(tvar))?
                        ivars=prefix(Ivars,       assoc_seq(ivar))?
                      methods=prefix(Methods,     assoc_seq(method_))?
                    prepended=prefix(Prepended,   seq(mixin))?
                     appended=prefix(Appended,    seq(mixin))?
                  RBrace
                { (fun env ->
                    let klass =
                      match Table.get env bind_as with
                      | Some (NamedClass klass)
                      -> klass
                      | Some _
                      -> assert false
                      | None
                      -> (let ancestor = Option.map (fun x -> x env) ancestor in
                          let klass = {
                            k_hash        = Hash_seed.make ();
                            k_name        = name;
                            k_ancestor    = ancestor;
                            k_metaclass   = metaclass env;
                            k_objectclass = None;
                            k_is_value    = Option.map_default (fun k -> k.k_is_value) false ancestor;
                            k_parameters  = Option.map_default (fun p -> p env) (Assoc.empty) parameters;
                            k_ivars       = Assoc.empty;
                            k_methods     = Assoc.empty;
                            k_prepended   = [];
                            k_appended    = [];
                          } in
                          Table.set env bind_as (NamedClass klass);
                          klass)
                    in
                    (fun () ->
                      Option.may (fun x -> klass.k_objectclass  <- Some (x env)) objectclass;
                      Option.may (fun x -> klass.k_ivars        <- x env) ivars;
                      Option.may (fun x -> klass.k_methods      <- x env) methods;
                      Option.may (fun x -> klass.k_prepended    <- x env) prepended;
                      Option.may (fun x -> klass.k_appended     <- x env) appended))
                }

              | bind_as=Name_Global Equal
                  Mixin name=Lit_String LBrace
                    metaclass=prefix(Metaclass,  klass)
                      methods=prefix(Methods,    assoc_seq(method_))?
                  RBrace
                { (fun env ->
                    let mixin = {
                      m_hash      = Hash_seed.make ();
                      m_name      = name;
                      m_metaclass = metaclass env;
                      m_methods   = Assoc.empty;
                    } in
                    Table.set env bind_as (NamedMixin mixin);
                    (fun () ->
                      Option.may (fun x -> mixin.m_methods <- x env) methods))
                }

              | bind_as=Name_Global Equal
                  Package name=Lit_String LBrace
                    metaclass=prefix(Metaclass,  klass)
                    constants=prefix(Constants,  table(value))?
                  RBrace
                { (fun env ->
                    let package = {
                      p_hash      = Hash_seed.make ();
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
                    local_env=prefix(Local_env,  local_env)
                     type_env=prefix(Type_env,   table(tvar))?
                    const_env=prefix(Const_env,  seq(package))?
                           ty=prefix(Type,       lambda_ty)
                         code=lambda_code
                  RBrace
                { (fun env ->
                    let args, body = code in
                    let lambda = {
                      l_hash      = Hash_seed.make ();
                      l_location  = loc;
                      l_ty        = dummy_lambda_ty;
                      l_local_env = dummy_local_env;
                      l_type_env  = Rt.tenv_create ();
                      l_const_env = Rt.cenv_empty;
                      l_args      = args;
                      l_body      = body;
                    } in
                    Table.set env bind_as (NamedLambda lambda);
                    (fun () ->
                      lambda.l_ty        <- ty env;
                      lambda.l_local_env <- local_env env;
                      Option.may (fun x -> lambda.l_type_env  <- (x env)) type_env;
                      Option.may (fun x -> lambda.l_const_env <- (x env)) const_env))
                }

              | bind_as=Name_Global Equal
                  Environment LBrace
                       parent=prefix(Parent,     local_env)?
                     bindings=prefix(Bindings,   table(lvar))?
                  RBrace
                { (fun env ->
                    let local_env = {
                      e_hash     = Hash_seed.make ();
                      e_parent   = Option.map (fun x -> x env) parent;
                      e_bindings = Table.create [];
                    } in
                    Table.set env bind_as (NamedLocalEnv local_env);
                    (fun () ->
                      Option.may (fun x -> Table.replace local_env.e_bindings (x env)) bindings))
                }

              | bind_as=Name_Global Equal
                  Instance klass=klass sp=assoc_ord(value) slots=table(value)
                { (fun env ->
                    let inst = {
                      i_hash  = Hash_seed.make ();
                      i_class = klass env, sp env;
                      i_slots = Table.create [];
                    } in
                    Table.set env bind_as (NamedInstance inst);
                    (fun () ->
                      Table.replace inst.i_slots (slots env)))
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
                      funcn, (fun () ->
                        let fixups = blocks (venv, funcn, fenv) in
                        List.iter (fun f -> f ()) fixups))
                }

   basic_block: id=Name_Label instrs=nonempty_list(instr)
                { (fun (venv, func, fenv) ->
                    let basic_block = create_block ~id func in
                      Table.set fenv id basic_block;
                      List.map (fun instr -> instr (venv, basic_block, fenv))
                        instrs) }

  basic_blocks: xs=nonempty_list(basic_block)
                { (fun env ->
                    List.concat (List.map (fun x -> x env) xs)) }

         local: id=Name_Local
                { (fun (venv, block, fenv) ->
                    try  Table.get_exn fenv id
                    with Not_found -> failwith (u"undefined %" ^ id)) }

       operand: x=local
                { x }
              | x=func
                { (fun (venv, block, fenv) -> x venv) }
              | x=value
                { (fun (venv, block, fenv) -> const (x venv)) }

 %public f_assoc(X,Y): x=X FatArrow y=Y
                        { x, y }

%public f_assocs(X,Y): LBrack xs=separated_nonempty_list(Comma, f_assoc(X, Y)) RBrack
                        { xs }

     assoc_ops: xs=f_assocs(operand, operand)
                { (fun env -> List.map (fun (x, y) -> x env, y env) xs) }

       phi_ops: xs=f_assocs(Name_Local, operand)
                { (fun ((venv, block, fenv) as env) ->
                    List.map (fun (id, value) ->
                      Table.get_exn fenv id, value env) xs) }

      spec_ops: xs=f_assocs(Lit_String, operand)
                { (fun env ->
                    Assoc.sorted (List.map (fun (param, value) ->
                                    param, value env) xs)) }

  local_env_op: x=local
                { x }
              | x=local_env
                { (fun (venv, block, fenv) ->
                      const (Rt.Environment (x venv))) }

       func_op: x=local
                { x }
              | x=func
                { (fun (venv, block, fenv) -> x venv) }

  opt_local_eq: id=Name_Local Equal
                { id }
              | /* nothing */
                { u"" }

         instr: id=opt_local_eq ty=ty? x=instr_body
                { (fun ((venv, block, fenv) as env) ->
                    let ty    = Option.map_default (fun ty -> ty venv) Rt.NilTy ty in
                    (* :( should fix this vvv *)
                    let iid    = if ty = Rt.NilTy then u"__unused" else id in
                    let instr = create_instr ~id:iid ty InvalidInstr in
                    append_instr instr block;

                    if Table.exists fenv id then
                      failwith (u"Duplicate name %" ^ id);
                    if id <> u"" then begin
                      if ty = Rt.NilTy then
                        failwith (u"Trying to assign id to a name of nil type");
                      Table.set fenv instr.id instr
                    end;

                    (fun () -> set_opcode instr (x env))) }

    instr_body: Phi operands=phi_ops
                { (fun env -> PhiInstr (operands env)) }
              | Frame parent=local_env_op
                { (fun env -> FrameInstr (parent env)) }
              | Lvar_load lenv=local_env_op Comma name=Lit_String
                { (fun env -> LVarLoadInstr (lenv env, name)) }
              | Lvar_store lenv=local_env_op Comma name=Lit_String Comma value=operand
                { (fun env -> LVarStoreInstr (lenv env, name, value env)) }
              | Ivar_load obj=operand Comma name=Lit_String
                { (fun env -> IVarLoadInstr (obj env, name)) }
              | Ivar_store obj=operand Comma name=Lit_String Comma value=operand
                { (fun env -> IVarStoreInstr (obj env, name, value env)) }
              | Primitive name=Lit_String args=args(operand)
                { (fun env -> PrimitiveInstr (name, args env)) }
              | Call func=func_op args=args(operand)
                { (fun env -> CallInstr (func env, args env)) }
              | Closure func=func_op Comma lenv=local_env_op
                { (fun env -> ClosureInstr (func env, lenv env)) }
              | Specialize cls=operand Comma operands=spec_ops
                { (fun env -> SpecializeInstr (cls env, operands env)) }
              | Tuple_extend  tup=operand Comma operands=seq(operand)
                { (fun env -> TupleExtendInstr (tup env, operands env)) }
              | Tuple_concat  tup=operand Comma other=operand
                { (fun env -> TupleConcatInstr (tup env, other env)) }
              | Record_extend re=operand Comma operands=assoc_ops
                { (fun env -> RecordExtendInstr (re env, operands env)) }
              | Record_concat re=operand Comma other=operand
                { (fun env -> RecordConcatInstr (re env, other env)) }
              | Jump target=operand
                { (fun env -> JumpInstr (target env)) }
              | Jump_if cond=operand Comma if_true=operand Comma if_false=operand
                { (fun env -> JumpIfInstr (cond env, if_true env, if_false env)) }
              | Return value=operand
                { (fun env -> ReturnInstr (value env) ) }

 overload_body: Map Function func=func FatArrow ty=ty target=func
                { (fun env capsule ->
                     Ssa.add_overload capsule (func env) (ty env) (target env)) }

     impl_body: Map Lambda meth=lambda FatArrow target=func
                { (fun env capsule ->
                     Ssa.add_lambda capsule (meth env) (target env))}

last_tvar_body: Map Tvar Equal tv=Lit_Integer
                { tv }

   definitions: env=definitions x=struct_body
                { env.g_fixups <- (x env.globals) :: env.g_fixups;
                  env }
              | env=definitions x=func_body
                { let funcn, fixup = x env.globals in
                  add_func env.capsule funcn;
                  env.c_fixups <- fixup :: env.c_fixups;
                  env }
              | env=definitions x=overload_body
              | env=definitions x=impl_body
                { x env.globals env.capsule;
                  env }
              | env=definitions x=last_tvar_body
                { env.last_tvar <- int_of_big_int x;
                  env }
              | /* empty */
                { { globals   = create_globals ();
                    g_fixups  = [];
                    capsule   = create_capsule ();
                    c_fixups  = [];
                    last_tvar = 0; } }

      toplevel: env=definitions EOF
                { List.iter (fun f -> f ()) (env.g_fixups @ env.c_fixups);
                  extract_roots env.last_tvar env.globals, env.capsule }
