open Unicode.Std
open Big_int
open Rt
open Ssa

let ordered = ref false

type named_value =
| NamedClass        of klass
| NamedMixin        of mixin
| NamedPackage      of package
| NamedInstance     of value Table.t (* compared by identity of their ivar tables *)
| NamedLambda       of lambda
| NamedLocalEnv     of local_env
| NamedSSAFunction  of func

module ValueIdentity =
struct
  type t = named_value

  let equal a b =
    match a, b with
    | NamedClass(a),        NamedClass(b)       -> a == b
    | NamedPackage(a),      NamedPackage(b)     -> a == b
    | NamedMixin(a),        NamedMixin(b)       -> a == b
    | NamedInstance(a),     NamedInstance(b)    -> a == b
    | NamedLambda(a),       NamedLambda(b)      -> a == b
    | NamedLocalEnv(a),     NamedLocalEnv(b)    -> a == b
    | NamedSSAFunction(a),  NamedSSAFunction(b) -> a == b
    | _, _ -> false

  let hash = Hashtbl.hash
end

module ValueEnvironment = IrEnvironment.Make(ValueIdentity)

(* Strings and identifiers *)

let escape_as_literal str =
  "\"" ^ (IrSupport.escaped str) ^ "\""

let escape_as_ident str =
  if IrSupport.is_printable str
  then str
  else escape_as_literal str

(* Locations *)

let string_of_loc (loc : Location.t) =
  let beg, nnd = (loc :> int * int)
  in "(" ^ (string_of_int beg) ^ " " ^ (string_of_int nnd) ^ ")"

(* Names *)

type ident =
| Global of string
| Local  of string

let string_of_ident ident =
  match ident with
  | Global ident -> "@" ^ (escape_as_ident ident)
  | Local  ident -> "%" ^ (escape_as_ident ident)

(* Environments *)

type env = {
        globals : ValueEnvironment.t;
  mutable image : string
}

let create_env () =
  {
    globals = ValueEnvironment.create ();
    image   = "";
  }

let bind env value name =
  let ir_bind env name =
    ValueEnvironment.bind env value name
  in
  match name with
  | Global name -> Global (ir_bind env.globals name)

(* Printer *)

let with_lookup env value name printer =
  match ValueEnvironment.lookup env.globals value with
  | Some name
  -> Global name
  | None
  -> (let name   = bind env value name in
      let assign = (string_of_ident name) ^ " = " ^ (printer ()) in
        env.image <- env.image ^ assign ^ "\n";
        name
      )

let string_of_table prefix table f =
  (String.concat ","
    (Table.map_list ~ordered:(!ordered) ~f:(fun key value ->
      "\n  " ^ prefix ^ (escape_as_literal key) ^ " = " ^ (f value))
    table)) ^ (if Table.empty table then "" else "\n" ^ prefix)

let string_of_seq elems xfrm =
  String.concat ", "
    (List.map xfrm elems)

let string_of_assoc elems xfrm =
  String.concat ", "
    (Table.map_list ~ordered:(!ordered) ~f:(fun key value ->
      (escape_as_literal key) ^ " = " ^ (xfrm value))
    elems)

let string_of_some is_empty f =
  if is_empty then
    ""
  else
    f ()

let rec string_of_value env value =
  match value with
  | Tvar(tv)          -> string_of_tvar env tv
  | Nil               -> "nil"
  | Truth             -> "true"
  | Lies              -> "false"
  | Integer(n)        -> "int " ^ (string_of_big_int n)
  | Symbol(s)         -> "symbol " ^ (escape_as_literal s)
  | Unsigned(w,v)     -> "unsigned(" ^ (string_of_int w) ^ ") " ^ (string_of_big_int v)
  | Signed(w,v)       -> "signed(" ^ (string_of_int w) ^ ") " ^ (string_of_big_int v)
  | Tuple(xs)         -> "[" ^ (string_of_seq xs (string_of_value env)) ^ "]"
  | Record(xs)        -> "{" ^ (string_of_assoc xs (string_of_value env)) ^ "}"
  | Environment(e)    -> string_of_ident (string_of_local_env env e)
  | Lambda(l)         -> "lambda " ^ (string_of_ident (string_of_lambda env l))
  | LambdaTy(lt)      -> string_of_lambda_ty env lt
  | Class(k,sp)       -> "class " ^ (string_of_ident (string_of_klass env k)) ^
                           "{" ^(string_of_assoc sp (string_of_value env))  ^ "}"
  | Mixin(k,sp)       -> "mixin " ^ (string_of_ident (string_of_mixin env k)) ^
                           "{" ^(string_of_assoc sp (string_of_value env))  ^ "}"
  | Package(p)        -> "package " ^ (string_of_ident (string_of_package env p))
  | Instance(c,iv)    -> "instance " ^ (string_of_ident (string_of_instance env c iv))

  | TvarTy | NilTy | BooleanTy | IntegerTy | SymbolTy | UnsignedTy _
  | SignedTy _ | TupleTy _ | RecordTy _ | EnvironmentTy _ | FunctionTy _
  | ClosureTy _ | BasicBlockTy
  -> "type " ^ (string_of_ty env value)

and string_of_ty env ty =
  match ty with
  | Tvar(tv)          -> string_of_tvar env tv
  | TvarTy            -> "tvar"
  | NilTy             -> "nil"
  | BooleanTy         -> "boolean"
  | IntegerTy         -> "int"
  | SymbolTy          -> "symbol"
  | UnsignedTy(w)     -> "unsigned(" ^ (string_of_int w) ^ ")"
  | SignedTy(w)       -> "signed(" ^ (string_of_int w) ^ ")"
  | TupleTy(xs)       -> "[" ^ (string_of_seq xs (string_of_ty env)) ^ "]"
  | RecordTy(xs)      -> "{" ^ (string_of_assoc xs (string_of_ty env)) ^ "}"
  | EnvironmentTy(x)  -> "environment " ^ (string_of_local_env_ty env x)
  | FunctionTy(xs,x)  -> "function (" ^ (string_of_seq xs (string_of_ty env)) ^ ") -> " ^
                            (string_of_ty env x)
  | ClosureTy(xs,x)   -> "closure (" ^ (string_of_seq xs (string_of_ty env)) ^ ") -> " ^
                            (string_of_ty env x)
  | BasicBlockTy      -> assert false
  | Class(k,sp)       -> "class " ^ (string_of_ident (string_of_klass env k)) ^
                           "{" ^(string_of_assoc sp (string_of_value env))  ^ "}"
  | _                 -> assert false (* interpolation *)

and string_of_klass env klass =
  with_lookup env (NamedClass klass) (Global ("c." ^ klass.k_name))
    (fun () ->
      "class " ^ (escape_as_literal klass.k_name) ^ " {\n" ^
        "  metaclass " ^ (string_of_ident (string_of_klass env klass.k_metaclass)) ^ "\n" ^
        (Option.map_default (fun klass ->
            "  ancestor " ^ (string_of_ident (string_of_klass env klass)) ^ "\n")
          "" klass.k_ancestor) ^
        (string_of_some (Table.empty klass.k_tvars) (fun () ->
          "  type_variables {" ^
            (string_of_table "  " klass.k_tvars (string_of_tvar env)) ^
          "}\n")) ^
        (string_of_some (Table.empty klass.k_ivars) (fun () ->
          "  instance_variables {" ^
            (string_of_table "  " klass.k_ivars (string_of_ivar env)) ^
          "}\n")) ^
        (string_of_some (Table.empty klass.k_methods) (fun () ->
          "  methods {" ^
            (string_of_table "  " klass.k_methods (string_of_method env)) ^
          "}\n")) ^
        (string_of_some (klass.k_prepended = []) (fun () ->
          "  prepended [" ^
            (string_of_seq klass.k_prepended
              (fun mixin -> string_of_ident (string_of_mixin env mixin))) ^ "]\n")) ^
        (string_of_some (klass.k_appended = []) (fun () ->
          "  appended ["  ^
            (string_of_seq klass.k_appended
              (fun mixin -> string_of_ident (string_of_mixin env mixin))) ^ "]\n")) ^
      "}\n")

and string_of_mixin env mixin =
  with_lookup env (NamedMixin mixin) (Global ("m." ^ mixin.m_name))
    (fun () ->
      "mixin " ^ (escape_as_literal mixin.m_name) ^ " {\n" ^
        "  metaclass " ^ (string_of_ident (string_of_klass env mixin.m_metaclass)) ^ "\n" ^
        (string_of_some (Table.empty mixin.m_methods) (fun () ->
          "  methods {" ^
            (string_of_table "  " mixin.m_methods (string_of_method env)) ^
          "}\n")) ^
      "}\n")

and string_of_tvar env (tvar:tvar) =
  "tvar(" ^ (string_of_int (tvar :> int)) ^ ")"

and string_of_lambda_ty env lambda_ty =
  "type lambda (" ^
    (string_of_ty env lambda_ty.l_args_ty) ^ ", " ^
    (string_of_ty env lambda_ty.l_kwargs_ty) ^ ") -> " ^
    (string_of_ty env lambda_ty.l_result_ty)

and string_of_ivar env ivar =
  let kind =
    match ivar.iv_kind with
    | Syntax.IVarImmutable   -> "immutable"
    | Syntax.IVarMutable     -> "mutable"
    | Syntax.IVarMetaMutable -> "meta_mutable"
  in
  (string_of_loc ivar.iv_location) ^ " " ^
    kind ^ " " ^
    (string_of_ty env ivar.iv_ty)

and string_of_method env meth =
  (if meth.im_dynamic then "dynamic " else "") ^
    (string_of_ident (string_of_lambda env meth.im_body))

and string_of_lambda env lam =
  with_lookup env (NamedLambda lam) (Global "") (fun () ->
    "lambda " ^ (string_of_loc lam.l_location) ^ " {\n" ^
      "  local_env " ^ (string_of_ident (string_of_local_env env lam.l_local_env)) ^ "\n" ^
      "  type_env {" ^
        (string_of_table "  " lam.l_type_env (string_of_tvar env)) ^
      "}\n" ^
      "  const_env [" ^
        (string_of_seq lam.l_const_env
          (fun pkg -> string_of_ident (string_of_package env pkg))) ^
      "]\n" ^
      "  type " ^ (string_of_lambda_ty env lam.l_ty) ^ "\n" ^
      "  args " ^ (Unicode.adopt_utf8s
          (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_formal_args lam.l_args))) ^ "\n" ^
      "  body " ^ (Unicode.adopt_utf8s
          (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_exprs lam.l_body))) ^ "\n" ^
    "}\n")

and string_of_local_env env lenv =
  with_lookup env (NamedLocalEnv lenv) (Global (""))
    (fun () ->
      "environment {\n" ^
        (Option.map_default (fun parent ->
            "  parent " ^ (string_of_ident (string_of_local_env env parent)) ^ "\n")
          "" lenv.e_parent) ^
        "  bindings " ^ (string_of_bindings env lenv.e_bindings) ^ "\n" ^
      "}\n")

and string_of_local_env_ty env ty =
  "{" ^ string_of_assoc ty.e_bindings_ty (fun b ->
    (string_of_loc b.b_location_ty) ^ " " ^
    (match b.b_kind_ty with
    | Syntax.LVarImmutable -> "immutable"
    | Syntax.LVarMutable   -> "mutable") ^ " " ^
    (string_of_ty env b.b_value_ty)) ^
  "}" ^
    (match ty.e_parent_ty with
    | Some parent_ty -> " -> " ^ (string_of_local_env_ty env parent_ty)
    | None -> "")

and string_of_bindings env bindings =
  "{" ^
    (string_of_table "  " bindings (fun b ->
      (string_of_loc b.b_location) ^ " " ^
      (match b.b_kind with
      | Syntax.LVarImmutable -> "immutable"
      | Syntax.LVarMutable   -> "mutable") ^ " " ^
      (string_of_value env b.b_value))) ^
  "}"

and string_of_bindings_ty env bindings =
  "{" ^
    (string_of_table "  " bindings (fun b ->
      (string_of_loc b.b_location_ty) ^ " " ^
      (match b.b_kind_ty with
      | Syntax.LVarImmutable -> "immutable"
      | Syntax.LVarMutable   -> "mutable") ^ " " ^
      (string_of_value env b.b_value_ty))) ^
  "}"

and string_of_package env package =
  with_lookup env (NamedPackage package) (Global ("p." ^ package.p_name))
    (fun () ->
      "package " ^ (escape_as_literal package.p_name) ^ " {\n" ^
        "  metaclass " ^ (string_of_ident (string_of_klass env package.p_metaclass)) ^ "\n" ^
        (string_of_some (Table.empty package.p_constants) (fun () ->
          "  constants {" ^
            (string_of_table "  " package.p_constants (string_of_value env)) ^
          "}\n"
        )) ^
      "}\n")

and string_of_instance env (klass, sp) ivars =
  with_lookup env (NamedInstance ivars) (Global "")
    (fun () ->
      "instance " ^ (string_of_ident (string_of_klass env klass)) ^
        "{" ^ (string_of_assoc sp (string_of_value env)) ^ "} {\n" ^
        (string_of_table "  " ivars (string_of_value env)) ^
      "}\n"
    )

let rec string_of_ssa_name env value =
  let print value =
    match value with
    | { opcode = Const value   } -> string_of_value env value
    | { opcode = Function func } -> string_of_ssa_name env value
    | _ -> string_of_ident (Local value.id)
  in
  let term opcode operands =
    opcode ^ " " ^ (String.concat ", " operands)
  in
  let instr opcode operands =
    (string_of_ident (Local value.id)) ^ " = " ^
      (string_of_ty env value.ty) ^ " " ^ (term opcode operands)
  in
  let call_like_instr callee operands =
    let prefix =
      if value.ty <> Rt.NilTy then
        (string_of_ident (Local value.id)) ^ " = " ^ (string_of_ty env value.ty) ^ " "
      else ""
    in prefix ^ callee ^ " (" ^ (String.concat ", " (List.map print operands)) ^ ")"
  in
  match value.opcode with
  | Function func ->
    (string_of_ident
      (with_lookup env (NamedSSAFunction func) (Global value.id)
        (fun () ->
          let ret_ty =
            match value.ty with
            | FunctionTy(args,ret) -> ret
            | _ -> assert false
          in
          "function (" ^
            (string_of_seq func.arguments
              (fun arg -> (string_of_ty env arg.ty) ^ " " ^ (print arg))) ^
          ") -> " ^
            (string_of_ty env ret_ty) ^ " {\n" ^
            (String.concat "\n"
              (List.map (string_of_ssa_name env) func.basic_blocks)) ^
          "}\n")))
  | BasicBlock block ->
    let preds   = List.map print (predecessors value) in
    let preds   = if preds <> [] then
                    " ; preds = " ^ (String.concat ", " preds)
                  else ""
    in
    let ident   = (escape_as_ident value.id) ^ ":" in
    let header  = ident ^
      (String.make (50 - (String.length ident)) (Char.of_string " ")) ^
      preds ^ "\n"
    in
    header ^
      (String.concat ""
        (List.map
          (fun v -> "  " ^ (string_of_ssa_name env v) ^ "\n")
          block.instructions))
  (* Handled in other places *)
  | Argument | Const _ ->
    assert false
  (* Instructions *)
  | InvalidInstr ->
    instr "$invalid" []
  | JumpInstr name ->
    term "jump" [print name]
  | JumpIfInstr (cond, if_true, if_false) ->
    term "jump_if" [print cond; print if_true; print if_false]
  | ReturnInstr name ->
    term "return" [print name]
  | PhiInstr operands ->
    instr "phi" (List.map (fun (block, value) ->
                  "[ " ^ (print block) ^ " => " ^ (print value) ^ " ]") operands)
  | FrameInstr (parent) ->
    instr "frame" [print parent]
  | LVarLoadInstr (env, var) ->
    instr "lvar_load" [print env; escape_as_literal var]
  | LVarStoreInstr (env, var, value) ->
    term "lvar_store" [print env; escape_as_literal var; print value]
  | CallInstr (func, operands) ->
    call_like_instr ("call " ^ (print func)) operands
  | MakeClosureInstr (func, env) ->
    instr "make_closure" [print func; print env]
  | CallClosureInstr (func, operands) ->
    call_like_instr ("call " ^ (print func)) operands
  | ResolveInstr (obj, meth) ->
    instr "resolve" [print obj; print meth]
  | PrimitiveInstr (name, operands) ->
    call_like_instr ("primitive " ^ (escape_as_literal name)) operands

let string_of_roots env roots =
  List.iter (fun klass -> ignore (string_of_klass env klass)) [
    roots.kClass;
    roots.kTypeVariable;
    roots.kNil;
    roots.kBoolean;
    roots.kInteger;
    roots.kSymbol;
    roots.kTuple;
    roots.kRecord;
    roots.kLambda;
    roots.kMixin;
    roots.kPackage
  ];
  ignore (string_of_package env roots.pToplevel)

let string_of_capsule env capsule =
  let funcs =
    if !ordered then
      List.sort (fun a b -> compare a.id b.id) capsule.functions
    else
      capsule.functions
  in
  List.iter (fun funcn -> ignore (string_of_ssa_name env funcn)) funcs;

  iter_overloads capsule ~f:(fun funcn args_ty funcn' ->
    let funcn   = string_of_ssa_name env funcn in
    let args_ty = string_of_seq args_ty (string_of_ty env) in
    let funcn'  = string_of_ssa_name env funcn' in
    env.image <- env.image ^
      "map function " ^ funcn ^ " (" ^ args_ty ^
          ") => " ^ funcn' ^ "\n")

let string_of ?roots capsule =
  let env = create_env () in
    Option.may (string_of_roots env) roots;
    string_of_capsule env capsule;

    env.image
