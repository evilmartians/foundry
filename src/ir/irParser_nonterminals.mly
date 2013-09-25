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
  | NamedStorage   of storage

  type definitions = {
            syntax    : Syntax.exprs Table.t;
            globals   : named_value Table.t;
    mutable g_fixups  : (unit -> unit) list;
    mutable last_tvar : int;

            capsule   : capsule;
    mutable locals    : Ssa.name Table.t;
    mutable c_fixups  : (unit -> unit) list;
  }

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
      (u"c.Array",              NamedClass roots.kArray);
      (u"c.meta:Array",         NamedClass roots.kArray.k_metaclass);
      (u"c.Lambda",             NamedClass roots.kLambda);
      (u"c.meta:Lambda",        NamedClass roots.kLambda.k_metaclass);
      (u"c.Mixin",              NamedClass roots.kMixin);
      (u"c.meta:Mixin",         NamedClass roots.kMixin.k_metaclass);
      (u"c.Package",            NamedClass roots.kPackage);
      (u"c.meta:Package",       NamedClass roots.kPackage.k_metaclass);
      (u"c.Memory",             NamedClass roots.kMemory);
      (u"c.meta:Memory",        NamedClass roots.kMemory.k_metaclass);
      (u"p.toplevel",           NamedPackage roots.pToplevel);
      (u"p.meta:toplevel",      NamedClass roots.pToplevel.p_metaclass);
    ]

  let extract_roots defs =
    let get_class name =
      match Table.get defs.globals name with
      | Some (NamedClass k) -> k
      | _ -> assert false
    and get_package name =
      match Table.get defs.globals name with
      | Some (NamedPackage p) -> p
      | _ -> assert false
    in
    {
      Rt.last_tvar  = defs.last_tvar;

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
      kArray        = get_class (u"c.Array");
      kLambda       = get_class (u"c.Lambda");
      kMixin        = get_class (u"c.Mixin");
      kPackage      = get_class (u"c.Package");
      kMemory       = get_class (u"c.Memory");

      pToplevel     = get_package (u"p.toplevel");
    }

  let dummy_lambda_ty = [], Rt.NilTy

  let dummy_local_env =
    { e_hash     = Hash_seed.make ();
      e_parent   = None;
      e_bindings = Table.create [] }

%}

%start <Rt.roots * Ssa.capsule> toplevel

%%
      %public table(X): LBrace
                          xs=separated_list(Comma,
                                separated_pair(Lit_String, Equal, X))
                        RBrace
                        {
                          (fun defs ->
                            let xs = List.map (fun (name, x) -> name, x defs) xs in
                            Table.create xs)
                        }

  %public assoc_seq(X): LBrace
                          xs=separated_list(Comma,
                                separated_pair(Lit_String, Equal, X))
                        RBrace
                        { (fun defs -> Assoc.sequental (List.map (fun (name, x) -> name, x defs) xs)) }

  %public assoc_ord(X): xs=assoc_seq(X)
                        { (fun defs -> Assoc.sort (xs defs)) }

        %public seq(X): LBrack
                          xs=separated_list(Comma, X)
                        RBrack
                        { (fun defs -> List.map (fun x -> x defs) xs) }

       %public args(X): LParen
                          xs=separated_list(Comma, X)
                        RParen
                        { (fun defs -> List.map (fun x -> x defs) xs) }

  %public prefix(X, Y): X y=Y
                        { y }

  %public f_assoc(X,Y): x=X FatArrow y=Y
                        { x, y }

 %public f_assocs(X,Y): LBrack xs=separated_nonempty_list(Comma, f_assoc(X, Y)) RBrack
                        { xs }

      location: LParen lft=Lit_Integer rgt=Lit_Integer RParen
                { Location.empty }

         exprs: name=Name_Syntax
                { (fun defs ->
                    match Table.get defs.syntax name with
                    | Some expr -> expr
                    | None -> failwith (u"Undefined #" ^ name)) }

          expr: exprs=exprs
                { (fun defs ->
                    match exprs defs with
                    | [expr] -> expr
                    | _ -> assert false) }

     expr_body: name=Name_Syntax Equal exprs=Syntax_Exprs
                { (fun defs ->
                    match Table.get defs.syntax name with
                    | Some v -> failwith (u"Duplicate name #" ^ name)
                    | None -> Table.set defs.syntax name exprs) }

        global: name=Name_Global
                { (fun defs ->
                    match Table.get defs.globals name with
                    | Some v -> v
                    | None -> failwith (u"Undefined @" ^ name)) }

     local_env: x=global
                { (fun defs ->
                    match x defs with
                    | NamedLocalEnv e -> e
                    | _ -> assert false)
                }
              | Empty
                { (fun defs -> dummy_local_env) }

        lambda: x=global
                { (fun defs ->
                    match x defs with
                    | NamedLambda l -> l
                    | _ -> assert false) }

         klass: x=global
                { (fun defs ->
                    match x defs with
                    | NamedClass k -> k
                    | _ -> assert false) }

         mixin: x=global
                { (fun defs ->
                    match x defs with
                    | NamedMixin m -> m
                    | _ -> assert false) }

       package: x=global
                { (fun defs ->
                    match x defs with
                    | NamedPackage p -> p
                    | _ -> assert false) }

      instance: x=global
                { (fun defs ->
                    match x defs with
                    | NamedInstance i -> i
                    | _ -> assert false) }

          func: x=global
                { (fun defs ->
                    match x defs with
                    | NamedFunction f -> f
                    | _ -> assert false) }

       storage: x=global
                { (fun defs ->
                    match x defs with
                    | NamedStorage a -> a
                    | _ -> assert false) }

          tvar: Tvar LParen x=Lit_Integer RParen
                { (fun defs -> Rt.adopt_tvar (int_of_big_int x)) }

lambda_ty_elem: ty=ty
                { (fun defs -> LambdaArg (ty defs)) }
              | Question ty=ty
                { (fun defs -> LambdaOptArg (ty defs)) }
              | Star ty=ty
                { (fun defs -> LambdaRest (ty defs)) }
              | kw=Lit_String Equal ty=ty
                { (fun defs -> LambdaKwArg (kw, ty defs)) }
              | Question kw=Lit_String Equal ty=ty
                { (fun defs -> LambdaKwOptArg (kw, ty defs)) }
              | StarStar ty=ty
                { (fun defs -> LambdaKwRest (ty defs)) }

     lambda_ty: Lambda args=args(lambda_ty_elem) Arrow result=ty
                { (fun defs -> args defs, result defs) }

environment_ty: Arrow xs=table(lvar_ty) parent=environment_ty
                { (fun defs -> Some {
                    e_ty_parent   = parent defs;
                    e_ty_bindings = xs defs;
                  }) }
              | /* nothing */
                { (fun defs -> None) }

      ty_const: Nil
                { (fun defs -> NilTy) }
              | Boolean
                { (fun defs -> BooleanTy) }
              | Int
                { (fun defs -> IntegerTy) }
              | Unsigned LParen w=Lit_Integer RParen
                { (fun defs -> UnsignedTy (int_of_big_int w)) }
              | Signed LParen w=Lit_Integer RParen
                { (fun defs -> SignedTy (int_of_big_int w)) }
              | Symbol
                { (fun defs -> SymbolTy) }
              | String
                { (fun defs -> StringTy) }
              | Option LParen x=ty RParen
                { (fun defs -> OptionTy (x defs)) }
              | xs=seq(ty)
                { (fun defs -> TupleTy (xs defs)) }
              | xs=assoc_ord(ty)
                { (fun defs -> RecordTy (xs defs)) }
              | Array LParen x=ty RParen
                { (fun defs -> ArrayTy (x defs)) }
              | Environment xs=table(lvar_ty) parent=environment_ty
                { (fun defs -> EnvironmentTy {
                    e_ty_parent   = parent defs;
                    e_ty_bindings = xs defs;
                  }) }
              | Class x=klass sp=assoc_ord(value)
                { (fun defs -> Class (x defs, sp defs)) }
              | x=lambda_ty
                { (fun defs -> LambdaTy (x defs)) }
              | Function args=args(ty) Arrow ty=ty
                { (fun defs -> FunctionTy (args defs, ty defs)) }

            ty: x=tvar
                { (fun defs -> Tvar (x defs)) }
              | Type Tvar
                { (fun defs -> TvarTy) }
              | x=ty_const
                { x }

         value: x=tvar
                { (fun defs -> Tvar (x defs)) }
              | Type Tvar
                { (fun defs -> TvarTy) }
              | Type x=ty_const
                { (fun defs -> (x defs)) }
              | Nil
                { (fun defs -> Nil) }
              | True
                { (fun defs -> Truth) }
              | False
                { (fun defs -> Lies) }
              | Int x=Lit_Integer
                { (fun defs -> Integer x) }
              | Unsigned LParen w=Lit_Integer RParen x=Lit_Integer
                { (fun defs -> Unsigned (int_of_big_int w, x)) }
              | Signed LParen w=Lit_Integer RParen x=Lit_Integer
                { (fun defs -> Signed (int_of_big_int w, x)) }
              | Symbol x=Lit_String
                { (fun defs -> Symbol x) }
              | String x=Lit_String
                { (fun defs -> String x) }
              | Option LParen x=value RParen
                { (fun defs -> Option (Full (x defs))) }
              | Option LParen Empty x=ty RParen
                { (fun defs -> Option (Empty (x defs))) }
              | xs=seq(value)
                { (fun defs -> Tuple (xs defs)) }
              | xs=assoc_ord(value)
                { (fun defs -> Record (xs defs)) }
              | Array LParen ty=ty RParen x=storage
                { (fun defs -> Array (ty defs, x defs)) }
              | Environment x=local_env
                { (fun defs -> Environment (x defs)) }
              | Lambda x=lambda
                { (fun defs -> Lambda (x defs)) }
              | Class x=klass sp=assoc_ord(value)
                { (fun defs -> Class (x defs, sp defs)) }
              | Mixin x=mixin sp=assoc_ord(value)
                { (fun defs -> Mixin (x defs, sp defs)) }
              | Package x=package
                { (fun defs -> Package (x defs)) }
              | Instance x=instance
                { (fun defs -> Instance (x defs)) }

     ivar_kind: Immutable
                { Syntax.IVarImmutable }
              | Mutable
                { Syntax.IVarMutable }
              | Meta_mutable
                { Syntax.IVarMetaMutable }

          ivar: loc=location kind=ivar_kind x=ty
                { (fun defs -> {
                    iv_hash     = Hash_seed.make ();
                    iv_location = loc;
                    iv_kind     = kind;
                    iv_ty       = x defs;
                  }) }

     lvar_kind: Immutable
                { Syntax.LVarImmutable }
              | Mutable
                { Syntax.LVarMutable }

          lvar: loc=location kind=lvar_kind x=value
                { (fun defs -> {
                    b_location = loc;
                    b_kind     = kind;
                    b_value    = x defs;
                  }) }

       lvar_ty: loc=location kind=lvar_kind x=ty
                { (fun defs -> {
                    b_ty_location = loc;
                    b_ty_kind     = kind;
                    b_ty          = x defs;
                  }) }

       method_: dynamic=boption(Dynamic) x=lambda
                { (fun defs -> {
                    im_hash    = Hash_seed.make ();
                    im_dynamic = dynamic;
                    im_body    = x defs;
                  }) }

lambda_arg_def: Default expr=expr
                { expr }

    lambda_arg: loc=location kind=lvar_kind name=Lit_String default=lambda_arg_def?
                { (fun defs -> {
                    la_location = loc;
                    la_kind     = kind;
                    la_name     = name;
                    la_default  = Option.map (fun x -> x defs) default;
                  }) }

   lambda_code: Args args=seq(lambda_arg) Body body=exprs
                { (fun defs -> args defs, body defs) }
              | lam=Syntax_Lambda
                { (fun defs ->
                    match lam with
                    | Syntax.Lambda (_, args, _, body)
                    -> Rt.lambda_args_of_formal_args args, [body]
                    | _
                    -> assert false) }

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
                { (fun defs ->
                    let klass =
                      match Table.get defs.globals bind_as with
                      | Some (NamedClass klass)
                      -> klass
                      | Some _
                      -> assert false
                      | None
                      -> (let ancestor = Option.map (fun x -> x defs) ancestor in
                          let klass = {
                            k_hash        = Hash_seed.make ();
                            k_name        = name;
                            k_ancestor    = ancestor;
                            k_metaclass   = metaclass defs;
                            k_objectclass = None;
                            k_is_value    = Option.map_default (fun k -> k.k_is_value) false ancestor;
                            k_parameters  = Option.map_default (fun p -> p defs) (Assoc.empty) parameters;
                            k_ivars       = Assoc.empty;
                            k_methods     = Assoc.empty;
                            k_prepended   = [];
                            k_appended    = [];
                          } in
                          Table.set defs.globals bind_as (NamedClass klass);
                          klass)
                    in
                    (fun () ->
                      Option.may (fun x -> klass.k_objectclass  <- Some (x defs)) objectclass;
                      Option.may (fun x -> klass.k_ivars        <- x defs) ivars;
                      Option.may (fun x -> klass.k_methods      <- x defs) methods;
                      Option.may (fun x -> klass.k_prepended    <- x defs) prepended;
                      Option.may (fun x -> klass.k_appended     <- x defs) appended))
                }

              | bind_as=Name_Global Equal
                  Mixin name=Lit_String LBrace
                    metaclass=prefix(Metaclass,  klass)
                      methods=prefix(Methods,    assoc_seq(method_))?
                  RBrace
                { (fun defs ->
                    let mixin = {
                      m_hash      = Hash_seed.make ();
                      m_name      = name;
                      m_metaclass = metaclass defs;
                      m_methods   = Assoc.empty;
                    } in
                    Table.set defs.globals bind_as (NamedMixin mixin);
                    (fun () ->
                      Option.may (fun x -> mixin.m_methods <- x defs) methods))
                }

              | bind_as=Name_Global Equal
                  Package name=Lit_String LBrace
                    metaclass=prefix(Metaclass,  klass)
                    constants=prefix(Constants,  table(value))?
                  RBrace
                { (fun defs ->
                    let package = {
                      p_hash      = Hash_seed.make ();
                      p_name      = name;
                      p_metaclass = metaclass defs;
                      p_constants = Table.create [];
                    } in
                    Table.set defs.globals bind_as (NamedPackage package);
                    (fun () ->
                      Option.may (fun x -> Table.replace package.p_constants (x defs)) constants))
                }

              | bind_as=Name_Global Equal
                  Lambda loc=location LBrace
                    local_env=prefix(Local_env,  local_env)
                     type_env=prefix(Type_env,   table(tvar))?
                    const_env=prefix(Const_env,  seq(package))?
                           ty=prefix(Type,       lambda_ty)
                         code=lambda_code
                  RBrace
                { (fun defs ->
                    let args, body = code defs in
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
                    Table.set defs.globals bind_as (NamedLambda lambda);
                    (fun () ->
                      lambda.l_ty        <- ty defs;
                      lambda.l_local_env <- local_env defs;
                      Option.may (fun x -> lambda.l_type_env  <- (x defs)) type_env;
                      Option.may (fun x -> lambda.l_const_env <- (x defs)) const_env))
                }

              | bind_as=Name_Global Equal
                  Environment LBrace
                       parent=prefix(Parent,     local_env)?
                     bindings=prefix(Bindings,   table(lvar))?
                  RBrace
                { (fun defs ->
                    let local_env = {
                      e_hash     = Hash_seed.make ();
                      e_parent   = Option.map (fun x -> x defs) parent;
                      e_bindings = Table.create [];
                    } in
                    Table.set defs.globals bind_as (NamedLocalEnv local_env);
                    (fun () ->
                      Option.may (fun x -> Table.replace local_env.e_bindings (x defs)) bindings))
                }

              | bind_as=Name_Global Equal
                  Instance klass=klass sp=assoc_ord(value) slots=table(value)
                { (fun defs ->
                    let inst = {
                      i_hash  = Hash_seed.make ();
                      i_class = klass defs, sp defs;
                      i_slots = Table.create [];
                    } in
                    Table.set defs.globals bind_as (NamedInstance inst);
                    (fun () ->
                      Table.replace inst.i_slots (slots defs)))
                }

              | bind_as=Name_Global Equal
                  Storage LParen capa=Lit_Integer X ty=ty RParen xs=seq(value)
                { (fun defs ->
                    let storage = {
                      st_hash     = Hash_seed.make ();
                      st_ty       = ty defs;
                      st_capacity = int_of_big_int capa;
                      st_elems    = DynArray.create ();
                    } in
                    Table.set defs.globals bind_as (NamedStorage storage);
                    (fun () ->
                      List.iter (DynArray.add storage.st_elems) (xs defs))) }

       fun_arg: ty=ty name=Name_Local
                { (fun defs -> ty defs, name) }

     func_body: bind_as=Name_Global Equal
                  Function args=args(fun_arg) Arrow result=ty LBrace
                    blocks=basic_blocks
                  RBrace
                {
                  (fun defs ->
                    let args  = args defs in
                    let funcn = create_func ~id:bind_as
                                  ~arg_ids:(List.map snd args)
                                  (List.map fst args) (result defs) in

                    Table.set defs.globals bind_as (NamedFunction funcn);
                    Ssa.add_func defs.capsule funcn;

                    (fun () ->
                      defs.locals <- Table.create [];
                      List.iter (fun arg -> Table.set defs.locals arg.id arg)
                        (func_of_name funcn).arguments;

                      List.iter (fun f -> f ()) (blocks defs funcn)))
                }

   basic_block: id=Name_Label instrs=nonempty_list(instr)
                { (fun defs funcn ->
                    let basic_block = create_block ~id funcn in
                    Table.set defs.locals id basic_block;
                    List.map (fun instr -> instr defs basic_block) instrs) }

  basic_blocks: xs=nonempty_list(basic_block)
                { (fun defs funcn ->
                    List.concat (List.map (fun x -> x defs funcn) xs)) }

         local: id=Name_Local
                { (fun defs ->
                    try  Table.get_exn defs.locals id
                    with Not_found -> failwith (u"undefined %" ^ id)) }

       operand: x=local
                { x }
              | x=func
                { (fun defs -> x defs) }
              | x=value
                { (fun defs -> const (x defs)) }

     assoc_ops: xs=f_assocs(operand, operand)
                { (fun defs -> List.map (fun (x, y) -> x defs, y defs) xs) }

       phi_ops: xs=f_assocs(Name_Local, operand)
                { (fun defs ->
                    List.map (fun (id, value) ->
                      Table.get_exn defs.locals id, value defs) xs) }

      spec_ops: xs=f_assocs(Lit_String, operand)
                { (fun defs ->
                    Assoc.sorted (List.map (fun (param, value) ->
                                    param, value defs) xs)) }

  local_env_op: x=local
                { x }
              | x=local_env
                { (fun defs -> const (Rt.Environment (x defs))) }

       func_op: x=local
                { x }
              | x=func
                { (fun defs -> x defs) }

  opt_local_eq: id=Name_Local Equal
                { id }
              | /* nothing */
                { u"" }

         instr: id=opt_local_eq ty=ty? x=instr_body
                { (fun defs basic_block ->
                    let ty    = Option.map_default (fun ty -> ty defs) Rt.NilTy ty in
                    (* :( should fix this vvv *)
                    let iid   = if ty = Rt.NilTy then u"__unused" else id in
                    let instr = create_instr ~id:iid ty InvalidInstr in
                    append_instr instr basic_block;

                    if Table.exists defs.locals id then
                      failwith (u"Duplicate name %" ^ id);

                    if id <> u"" then begin
                      if ty = Rt.NilTy then
                        failwith (u"Trying to assign id to a name of nil type");
                      Table.set defs.locals instr.id instr
                    end;

                    (fun () -> set_opcode instr (x defs))) }

    instr_body: Phi operands=phi_ops
                { (fun defs -> PhiInstr (operands defs)) }
              | Select cond=operand Comma if_true=operand Comma if_false=operand
                { (fun defs -> SelectInstr (cond defs, if_true defs, if_false defs)) }
              | Frame parent=local_env_op
                { (fun defs -> FrameInstr (parent defs)) }
              | Lvar_load lenv=local_env_op Comma name=Lit_String
                { (fun defs -> LVarLoadInstr (lenv defs, name)) }
              | Lvar_store lenv=local_env_op Comma name=Lit_String Comma value=operand
                { (fun defs -> LVarStoreInstr (lenv defs, name, value defs)) }
              | Ivar_load obj=operand Comma name=Lit_String
                { (fun defs -> IVarLoadInstr (obj defs, name)) }
              | Ivar_store obj=operand Comma name=Lit_String Comma value=operand
                { (fun defs -> IVarStoreInstr (obj defs, name, value defs)) }
              | Primitive name=Lit_String args=args(operand)
                { (fun defs -> PrimitiveInstr (name, args defs)) }
              | Call func=func_op args=args(operand)
                { (fun defs -> CallInstr (func defs, args defs)) }
              | Closure func=func_op Comma lenv=local_env_op
                { (fun defs -> ClosureInstr (func defs, lenv defs)) }
              | Specialize cls=operand Comma operands=spec_ops
                { (fun defs -> SpecializeInstr (cls defs, operands defs)) }
              | Tuple_extend  tup=operand Comma operands=seq(operand)
                { (fun defs -> TupleExtendInstr (tup defs, operands defs)) }
              | Tuple_concat  tup=operand Comma other=operand
                { (fun defs -> TupleConcatInstr (tup defs, other defs)) }
              | Record_extend re=operand Comma operands=assoc_ops
                { (fun defs -> RecordExtendInstr (re defs, operands defs)) }
              | Record_concat re=operand Comma other=operand
                { (fun defs -> RecordConcatInstr (re defs, other defs)) }
              | Jump target=operand
                { (fun defs -> JumpInstr (target defs)) }
              | Jump_if cond=operand Comma if_true=operand Comma if_false=operand
                { (fun defs -> JumpIfInstr (cond defs, if_true defs, if_false defs)) }
              | Return value=operand
                { (fun defs -> ReturnInstr (value defs) ) }

 overload_body: Map Function func=func FatArrow ty=ty target=func
                { (fun defs ->
                     Ssa.add_overload defs.capsule (func defs) (ty defs) (target defs)) }

     impl_body: Map Lambda meth=lambda FatArrow target=func
                { (fun defs ->
                     Ssa.add_lambda defs.capsule (meth defs) (target defs))}

last_tvar_body: Map Tvar Equal tv=Lit_Integer
                { tv }

   definitions: defs=definitions x=expr_body
                { x defs;
                  defs }
              | defs=definitions x=struct_body
                { defs.g_fixups <- (x defs) :: defs.g_fixups;
                  defs }
              | defs=definitions x=func_body
                { defs.c_fixups <- (x defs) :: defs.c_fixups;
                  defs }
              | defs=definitions x=overload_body
              | defs=definitions x=impl_body
                { x defs;
                  defs }
              | defs=definitions x=last_tvar_body
                { defs.last_tvar <- int_of_big_int x;
                  defs }
              | /* empty */
                { { syntax    = Table.create [];
                    globals   = create_globals ();
                    last_tvar = 0;
                    g_fixups  = [];
                    capsule   = create_capsule ();
                    locals    = Table.create [];
                    c_fixups  = []; } }

      toplevel: defs=definitions EOF
                { List.iter (fun f -> f ()) (defs.g_fixups @ defs.c_fixups);
                  extract_roots defs, defs.capsule }
