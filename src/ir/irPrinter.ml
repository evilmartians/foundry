open Unicode.Std
open Big_int
open Rt
open Ssa

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

let print_string str =
  "\"" ^ (IrSupport.escaped str) ^ "\""

let print_ident str =
  if IrSupport.is_printable str
  then str
  else print_string str

(* Locations *)

let print_loc (loc : Location.t) =
  let beg, nnd = (loc :> int * int)
  in "(" ^ (string_of_int beg) ^ " " ^ (string_of_int nnd) ^ ")"

(* Names *)

type name =
| Global of string
| Local  of string

let print_name name =
  match name with
  | Global name -> "@" ^ (print_ident name)
  | Local  name -> "%" ^ (print_ident name)

let mangle_name ?(prefix="") ?(suffix="") name =
  match name with
  | Global name -> Global (prefix ^ name ^ suffix)
  | Local  name -> Local  (prefix ^ name ^ suffix)

(* Environments *)

type env = {
         global : ValueEnvironment.t;
          local : ValueEnvironment.t;
  mutable image : string
}

let create_env () =
  {
    global = ValueEnvironment.create ();
    local  = ValueEnvironment.create ();
    image  = "";
  }

let bind env value name =
  let ir_bind env name =
    ValueEnvironment.bind env value name
  in
  match name with
  | Global name -> Global (ir_bind env.global name)
  | Local  name -> Local  (ir_bind env.local name)

(* Printer *)

let with_lookup env value name printer =
  match ValueEnvironment.get env.global value with
  | Some name
  -> Global name
  | None
  -> (let name   = bind env value name in
      let assign = (print_name name) ^ " = " ^ (printer ()) in
        env.image <- env.image ^ assign ^ "\n";
        name
      )

let print_table prefix table f =
  (String.concat ","
    (Table.map_list (fun key value ->
      "\n  " ^ prefix ^ (print_string key) ^ " = " ^ (f value))
    table)) ^ (if Table.empty table then "" else "\n" ^ prefix)

let print_seq elems xfrm =
  String.concat ", "
    (List.map xfrm elems)

let print_assoc elems xfrm =
  String.concat ", "
    (Table.map_list (fun key value ->
      (print_string key) ^ " = " ^ (xfrm value))
    elems)

let print_some is_empty f =
  if is_empty then
    ""
  else
    f ()

let rec print_value env value =
  match value with
  | Tvar(tv)          -> print_tvar env tv
  | TvarTy            -> "type tvar"
  | Nil               -> "nil"
  | NilTy             -> "type nil"
  | Truth             -> "true"
  | Lies              -> "false"
  | BooleanTy         -> "type boolean"
  | Integer(n)        -> "int " ^ (string_of_big_int n)
  | IntegerTy         -> "type int"
  | Symbol(s)         -> "symbol " ^ (print_string s)
  | SymbolTy          -> "type symbol"
  | Tuple(xs)         -> "[" ^ (print_seq xs (print_value env)) ^ "]"
  | TupleTy(xs)       -> "type [" ^ (print_seq xs (print_value env)) ^ "]"
  | Record(xs)        -> "{" ^ (print_assoc xs (print_value env)) ^ "}"
  | RecordTy(xs)      -> "type {" ^ (print_assoc xs (print_value env)) ^ "}"
  | Environment(e)    -> "environment " ^ (print_name (print_local_env env e))
  | EnvironmentTy(et) -> assert false (* not yet *)
  | Lambda(l)         -> "lambda " ^ (print_name (print_lambda env l))
  | LambdaTy(lt)      -> print_lambda_ty env lt
  | Class(k,sp)       -> "class " ^ (print_name (print_klass env k)) ^
                           "{" ^(print_assoc sp (print_value env))  ^ "}"
  | Mixin(k,sp)       -> "mixin " ^ (print_name (print_mixin env k)) ^
                           "{" ^(print_assoc sp (print_value env))  ^ "}"
  | Package(p)        -> "package " ^ (print_name (print_package env p))
  | Instance(c,iv)    -> "instance " ^ (print_name (print_instance env c iv))
  | FunctionTy(_,_)   -> assert false
  | BasicBlockTy      -> assert false

and print_klass env klass =
  with_lookup env (NamedClass klass) (Global ("c." ^ klass.k_name))
    (fun () ->
      "class " ^ (print_string klass.k_name) ^ " {\n" ^
        "  metaclass " ^ (print_name (print_klass env klass.k_metaclass)) ^ "\n" ^
        (Option.map_default (fun klass ->
            "  ancestor " ^ (print_name (print_klass env klass)) ^ "\n")
          "" klass.k_ancestor) ^
        (print_some (Table.empty klass.k_tvars) (fun () ->
          "  type_variables {" ^
            (print_table "  " klass.k_tvars (print_tvar env)) ^
          "}\n")) ^
        (print_some (Table.empty klass.k_ivars) (fun () ->
          "  instance_variables {" ^
            (print_table "  " klass.k_ivars (print_ivar env)) ^
          "}\n")) ^
        (print_some (Table.empty klass.k_methods) (fun () ->
          "  methods {" ^
            (print_table "  " klass.k_methods (print_method env)) ^
          "}\n")) ^
        (print_some (klass.k_prepended = []) (fun () ->
          "  prepended [" ^
            (print_seq klass.k_prepended
              (fun mixin -> print_name (print_mixin env mixin))) ^ "]\n")) ^
        (print_some (klass.k_appended = []) (fun () ->
          "  appended ["  ^
            (print_seq klass.k_appended
              (fun mixin -> print_name (print_mixin env mixin))) ^ "]\n")) ^
      "}")

and print_mixin env mixin =
  with_lookup env (NamedMixin mixin) (Global ("m." ^ mixin.m_name))
    (fun () ->
      "mixin " ^ (print_string mixin.m_name) ^ " {\n" ^
        "  metaclass " ^ (print_name (print_klass env mixin.m_metaclass)) ^ "\n" ^
        (print_some (Table.empty mixin.m_methods) (fun () ->
          "  methods {" ^
            (print_table "  " mixin.m_methods (print_method env)) ^
          "}\n")) ^
      "}")

and print_tvar env (tvar:tvar) =
  "tvar " ^ (string_of_int (tvar :> int))

and print_lambda_ty env lambda_ty =
  "type lambda (" ^
    (print_value env lambda_ty.l_args_ty) ^ ", " ^
    (print_value env lambda_ty.l_kwargs_ty) ^ ") -> " ^
    (print_value env lambda_ty.l_result_ty)

and print_ivar env ivar =
  let kind =
    match ivar.iv_kind with
    | Syntax.IVarImmutable   -> "immutable"
    | Syntax.IVarMutable     -> "mutable"
    | Syntax.IVarMetaMutable -> "meta_mutable"
  in
  (print_loc ivar.iv_location) ^ " " ^
    kind ^ " " ^
    (print_value env ivar.iv_ty)

and print_method env meth =
  (if meth.im_dynamic then "dynamic " else "") ^
    (print_name (print_lambda env meth.im_body))

and print_lambda env lam =
  with_lookup env (NamedLambda lam) (Global "") (fun () ->
    "lambda " ^ (print_loc lam.l_location) ^ " {\n" ^
      "  local_env " ^ (print_name (print_local_env env lam.l_local_env)) ^ "\n" ^
      "  type_env {" ^
        (print_table "  " lam.l_type_env (print_tvar env)) ^
      "}\n" ^
      "  const_env [" ^
        (print_seq lam.l_const_env
          (fun pkg -> print_name (print_package env pkg))) ^
      "]\n" ^
      "  type " ^ (print_lambda_ty env lam.l_ty) ^ "\n" ^
      "  args " ^ (Unicode.adopt_utf8s
          (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_formal_args lam.l_args))) ^ "\n" ^
      "  body " ^ (Unicode.adopt_utf8s
          (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_exprs lam.l_body))) ^ "\n" ^
    "}")

and print_local_env env lenv =
  with_lookup env (NamedLocalEnv lenv) (Global (""))
    (fun () ->
      "environment {\n" ^
        (Option.map_default (fun parent ->
            "  parent " ^ (print_name (print_local_env env parent)) ^ "\n")
          "" lenv.e_parent) ^
        "  bindings " ^ (print_bindings env lenv.e_bindings) ^ "\n" ^
      "}")

and print_bindings env bindings =
  "{" ^
    (print_table "  " bindings (fun b ->
      (print_loc b.b_location) ^ " " ^
      (match b.b_kind with
      | Syntax.LVarImmutable -> "immutable"
      | Syntax.LVarMutable   -> "mutable") ^ " " ^
      (print_value env b.b_value))) ^
  "}"

and print_bindings_ty env bindings =
  "{" ^
    (print_table "  " bindings (fun b ->
      (print_loc b.b_location_ty) ^ " " ^
      (match b.b_kind_ty with
      | Syntax.LVarImmutable -> "immutable"
      | Syntax.LVarMutable   -> "mutable") ^ " " ^
      (print_value env b.b_value_ty))) ^
  "}"

and print_package env package =
  with_lookup env (NamedPackage package) (Global ("p." ^ package.p_name))
    (fun () ->
      "package " ^ (print_string package.p_name) ^ " {\n" ^
        "  metaclass " ^ (print_name (print_klass env package.p_metaclass)) ^ "\n" ^
        (print_some (Table.empty package.p_constants) (fun () ->
          "  constants {" ^
            (print_table "  " package.p_constants (print_value env)) ^
          "}\n"
        )) ^
      "}")

and print_instance env (klass, sp) ivars =
  with_lookup env (NamedInstance ivars) (Global "")
    (fun () ->
      "instance " ^ (print_name (print_klass env klass)) ^
        "{" ^ (print_assoc sp (print_value env)) ^ "} {\n" ^
        (print_table "  " ivars (print_value env)) ^
      "}"
    )

let rec print_ssa_value env value =
  let print value =
    match value with
    | { opcode = Const value } -> print_value env value
    | _                        -> print_name (Local value.id)
  in
  let term opcode operands =
    opcode ^ " " ^ (String.concat ", " operands)
  in
  let insn opcode operands =
    (print_name (Local value.id)) ^ " = " ^ (term opcode operands)
  in
  match value.opcode with
  | Function func ->
    (print_name
      (with_lookup env (NamedSSAFunction func) (Global value.id)
        (fun () ->
          let ret_ty =
            match value.ty with
            | FunctionTy(args,ret) -> ret
            | _ -> assert false
          in
          "function (" ^
            (print_seq func.arguments
              (fun arg -> (print_value env arg.ty) ^ " " ^ (print arg))) ^
          ") -> " ^
            (print_value env ret_ty) ^ " {\n" ^
            (String.concat "\n"
              (List.map (print_ssa_value env) func.basic_blocks)) ^
          "}")))
  | BasicBlock block ->
    (print_ident value.id) ^ ":\n" ^
      (String.concat ""
        (List.map
          (fun v -> "  " ^ (print_ssa_value env v) ^ "\n")
          block.instructions))
  (* Handled in other places *)
  | Argument | Const _ ->
    assert false
  (* Instructions *)
  | JumpInsn name ->
    term "jump" [print name]
  | JumpIfInsn (cond, if_true, if_false) ->
    term "jump_if" [print cond; print if_true; print if_false]
  | ReturnInsn name ->
    term "return" [print name]
  | EnvironmentInsn (bindings, parent) ->
    insn "environment" [print_bindings_ty env bindings; print parent]
  | LVarLoadInsn (env, var) ->
    insn "lvar_load" [print env; print_string var]
  | LVarStoreInsn (env, var, value) ->
    insn "lvar_store" [print env; print_string var; print value]

let print_roots roots =
  let env = create_env () in
    List.iter (fun klass -> ignore (print_klass env klass)) [
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
    print_package env roots.pToplevel;
    env.image

let print_ssa value =
  let env = create_env () in
    print_ssa_value env value;
    env.image
