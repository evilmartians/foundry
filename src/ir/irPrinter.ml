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
  match ValueEnvironment.lookup env.global value with
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
    (Table.map_list ~ordered:(!ordered) ~f:(fun key value ->
      "\n  " ^ prefix ^ (print_string key) ^ " = " ^ (f value))
    table)) ^ (if Table.empty table then "" else "\n" ^ prefix)

let print_seq elems xfrm =
  String.concat ", "
    (List.map xfrm elems)

let print_assoc elems xfrm =
  String.concat ", "
    (Table.map_list ~ordered:(!ordered) ~f:(fun key value ->
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
  | Nil               -> "nil"
  | Truth             -> "true"
  | Lies              -> "false"
  | Integer(n)        -> "int " ^ (string_of_big_int n)
  | Symbol(s)         -> "symbol " ^ (print_string s)
  | Unsigned(w,v)     -> "unsigned(" ^ (string_of_int w) ^ ") " ^ (string_of_big_int v)
  | Signed(w,v)       -> "signed(" ^ (string_of_int w) ^ ") " ^ (string_of_big_int v)
  | Tuple(xs)         -> "[" ^ (print_seq xs (print_value env)) ^ "]"
  | Record(xs)        -> "{" ^ (print_assoc xs (print_value env)) ^ "}"
  | Environment(e)    -> "environment " ^ (print_name (print_local_env env e))
  | Lambda(l)         -> "lambda " ^ (print_name (print_lambda env l))
  | LambdaTy(lt)      -> print_lambda_ty env lt
  | Class(k,sp)       -> "class " ^ (print_name (print_klass env k)) ^
                           "{" ^(print_assoc sp (print_value env))  ^ "}"
  | Mixin(k,sp)       -> "mixin " ^ (print_name (print_mixin env k)) ^
                           "{" ^(print_assoc sp (print_value env))  ^ "}"
  | Package(p)        -> "package " ^ (print_name (print_package env p))
  | Instance(c,iv)    -> "instance " ^ (print_name (print_instance env c iv))

  | TvarTy | NilTy | BooleanTy | IntegerTy | SymbolTy | UnsignedTy _
  | SignedTy _ | TupleTy _ | RecordTy _ | EnvironmentTy _ | FunctionTy _
  | BasicBlockTy
  -> "type " ^ (print_ty env value)

and print_ty env ty =
  match ty with
  | Tvar(tv)          -> print_tvar env tv
  | TvarTy            -> "tvar"
  | NilTy             -> "nil"
  | BooleanTy         -> "boolean"
  | IntegerTy         -> "int"
  | SymbolTy          -> "symbol"
  | UnsignedTy(w)     -> "unsigned(" ^ (string_of_int w) ^ ")"
  | SignedTy(w)       -> "signed(" ^ (string_of_int w) ^ ")"
  | TupleTy(xs)       -> "[" ^ (print_seq xs (print_ty env)) ^ "]"
  | RecordTy(xs)      -> "{" ^ (print_assoc xs (print_ty env)) ^ "}"
  | EnvironmentTy(x)  -> "environment " ^ (print_local_env_ty env x)
  | FunctionTy(xs,x)  -> "(" ^ (print_seq xs (print_ty env)) ^ ") -> " ^ (print_ty env x)
  | BasicBlockTy      -> assert false
  | _                 -> assert false (* TODO interpolation *)

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
  "tvar(" ^ (string_of_int (tvar :> int)) ^ ")"

and print_lambda_ty env lambda_ty =
  "type lambda (" ^
    (print_ty env lambda_ty.l_args_ty) ^ ", " ^
    (print_ty env lambda_ty.l_kwargs_ty) ^ ") -> " ^
    (print_ty env lambda_ty.l_result_ty)

and print_ivar env ivar =
  let kind =
    match ivar.iv_kind with
    | Syntax.IVarImmutable   -> "immutable"
    | Syntax.IVarMutable     -> "mutable"
    | Syntax.IVarMetaMutable -> "meta_mutable"
  in
  (print_loc ivar.iv_location) ^ " " ^
    kind ^ " " ^
    (print_ty env ivar.iv_ty)

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

and print_local_env_ty env ty =
  "{" ^ print_assoc ty.e_bindings_ty (fun b ->
    (print_loc b.b_location_ty) ^ " " ^
    (match b.b_kind_ty with
    | Syntax.LVarImmutable -> "immutable"
    | Syntax.LVarMutable   -> "mutable") ^ " " ^
    (print_ty env b.b_value_ty)) ^
  "}" ^
    (match ty.e_parent_ty with
    | Some parent_ty -> " -> " ^ (print_local_env_ty env parent_ty)
    | None -> "")

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
    | { opcode = Const value   } -> print_value env value
    | { opcode = Function func } -> print_ssa_value env value
    | _ -> print_name (Local value.id)
  in
  let term opcode operands =
    opcode ^ " " ^ (String.concat ", " operands)
  in
  let instr opcode operands =
    (print_name (Local value.id)) ^ " = " ^
      (print_ty env value.ty) ^ " " ^ (term opcode operands)
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
              (fun arg -> (print_ty env arg.ty) ^ " " ^ (print arg))) ^
          ") -> " ^
            (print_ty env ret_ty) ^ " {\n" ^
            (String.concat "\n"
              (List.map (print_ssa_value env) func.basic_blocks)) ^
          "}\n")))
  | BasicBlock block ->
    let preds   = List.map print (predecessors value) in
    let preds   = if preds <> [] then
                    " ; preds = " ^ (String.concat ", " preds)
                  else ""
    in
    let ident   = (print_ident value.id) ^ ":" in
    let header  = ident ^
      (String.make (50 - (String.length ident)) (Char.of_string " ")) ^
      preds ^ "\n"
    in
    header ^
      (String.concat ""
        (List.map
          (fun v -> "  " ^ (print_ssa_value env v) ^ "\n")
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
    instr "lvar_load" [print env; print_string var]
  | LVarStoreInstr (env, var, value) ->
    term "lvar_store" [print env; print_string var; print value]
  | CallInstr (func, operands) ->
    let prefix =
      if value.ty <> Rt.NilTy then
        (print_name (Local value.id)) ^ " = " ^ (print_ty env value.ty) ^ " "
      else ""
    in prefix ^ "call " ^ (print func) ^
          " (" ^ (String.concat ", " (List.map print operands)) ^ ")"
  | PrimitiveInstr (name, operands) ->
    let prefix =
      if value.ty <> Rt.NilTy then
        (print_name (Local value.id)) ^ " = " ^ (print_ty env value.ty) ^ " "
      else ""
    in prefix ^ "primitive " ^ (print_string name) ^
          " (" ^ (String.concat ", " (List.map print operands)) ^ ")"

let print roots capsule =
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
    ignore (print_package env roots.pToplevel);

    List.iter (fun funcn ->
        ignore (print_ssa_value env funcn))
      capsule.functions;

    env.image
