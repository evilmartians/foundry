open Unicode.Std
open Big_int
open Rt
open Ssa

let ordered = ref false

(* Strings and identifiers *)

let escape_as_literal str =
  "\"" ^ (IrSupport.escaped str) ^ "\""

let escape_as_ident str =
  if IrSupport.is_printable str
  then str
  else escape_as_literal str

(* Locations *)

let string_of_loc loc =
  let beg, nnd = (loc : Location.t :> int * int)
  in "(" ^ (string_of_int beg) ^ " " ^ (string_of_int nnd) ^ ")"

(* Serializer state *)

type state = {
          symtab  : Symtab.t;
          globals : string Valuetbl.t;
  mutable image   : string
}

let create_state capsule_symtab =
  { symtab  = Symtab.copy capsule_symtab;
    globals = Valuetbl.create 10;
    image   = ""; }

let bind state value id printer =
  let id =
    try
      Valuetbl.find state.globals value
    with Not_found ->
      (* Assign a fresh unique identifier. *)
      let id = Symtab.add state.symtab id in
      Valuetbl.add state.globals value id;
      (* Append serialized entity to the image. *)
      let entity = "@" ^ (escape_as_ident id) ^ " = " ^ (printer ()) in
      state.image <- state.image ^ entity ^ "\n";
      id
  in "@" ^ (escape_as_ident id)

(* Basic operations *)

let string_of_some is_empty f =
  if is_empty then
    ""
  else
    f ()

let string_of_seq elems xfrm =
  String.concat ", "
    (List.map xfrm elems)

let string_of_table prefix table xfrm =
  (String.concat ","
    (Table.map_list ~ordered:(!ordered) ~f:(fun key value ->
      "\n  " ^ prefix ^ (escape_as_literal key) ^ " = " ^ (xfrm value))
    table)) ^ (if Table.is_empty table then "" else "\n" ^ prefix)

let string_of_assoc prefix elems xfrm =
  (String.concat ", "
    (Assoc.map_list (fun key value ->
      "\n  " ^ prefix ^ (escape_as_literal key) ^ " = " ^ (xfrm value))
    elems)) ^ (if Assoc.is_empty elems then "" else "\n" ^ prefix)

let string_of_assoc_inline elems xfrm =
  String.concat ", "
    (Assoc.map_list ~f:(fun key value ->
      (escape_as_literal key) ^ " = " ^ (xfrm value))
    elems)

(* Entity printers *)

let rec string_of_value state value =
  match value with
  | Tvar(tv)          -> string_of_tvar state tv
  | Nil               -> "nil"
  | Truth             -> "true"
  | Lies              -> "false"
  | Integer(n)        -> "int " ^ (string_of_big_int n)
  | Symbol(s)         -> "symbol " ^ (escape_as_literal s)
  | String(s)         -> "string " ^ (escape_as_literal s)
  | Unsigned(w,v)     -> "unsigned(" ^ (string_of_int w) ^ ") " ^ (string_of_big_int v)
  | Signed(w,v)       -> "signed(" ^ (string_of_int w) ^ ") " ^ (string_of_big_int v)
  | Tuple(xs)         -> "[" ^ (string_of_seq xs (string_of_value state)) ^ "]"
  | Record(xs)        -> "{" ^ (string_of_assoc_inline xs (string_of_value state)) ^ "}"
  | Environment(e)    ->  string_of_local_env state e
  | Lambda(l)         -> "lambda " ^ (string_of_lambda state l)
  | Class(k,sp)       -> "class " ^ (string_of_klass state k) ^
                           "{" ^ (string_of_assoc_inline sp (string_of_value state))  ^ "}"
  | Mixin(k,sp)       -> "mixin " ^ (string_of_mixin state k) ^
                           "{" ^(string_of_assoc_inline sp (string_of_value state))  ^ "}"
  | Package(p)        -> "package " ^ (string_of_package state p)
  | Instance(c,iv)    -> "instance " ^ (string_of_instance state c iv)

  | TvarTy | NilTy | BooleanTy | IntegerTy | SymbolTy | StringTy | UnsignedTy _
  | SignedTy _ | TupleTy _ | RecordTy _ | LambdaTy _ | EnvironmentTy _ | FunctionTy _
  | ClosureTy _ | BasicBlockTy
  -> "type " ^ (string_of_ty state value)

and string_of_ty state ty =
  match ty with
  | Tvar(tv)          -> string_of_tvar state tv
  | TvarTy            -> "tvar"
  | NilTy             -> "nil"
  | BooleanTy         -> "boolean"
  | IntegerTy         -> "int"
  | SymbolTy          -> "symbol"
  | StringTy          -> "string"
  | UnsignedTy(w)     -> "unsigned(" ^ (string_of_int w) ^ ")"
  | SignedTy(w)       -> "signed(" ^ (string_of_int w) ^ ")"
  | TupleTy(xs)       -> "[" ^ (string_of_seq xs (string_of_ty state)) ^ "]"
  | RecordTy(xs)      -> "{" ^ (string_of_assoc_inline xs (string_of_ty state)) ^ "}"
  | LambdaTy(lt)      -> string_of_lambda_ty state lt
  | EnvironmentTy(x)  -> "environment " ^ (string_of_local_env_ty state x)
  | FunctionTy(xs,x)  -> "function (" ^ (string_of_seq xs (string_of_ty state)) ^ ") -> " ^
                            (string_of_ty state x)
  | ClosureTy(xs,x)   -> "closure (" ^ (string_of_seq xs (string_of_ty state)) ^ ") -> " ^
                            (string_of_ty state x)
  | BasicBlockTy      -> assert false
  | Class(k,sp)       -> "class " ^ (string_of_klass state k) ^
                           "{" ^(string_of_assoc_inline sp (string_of_value state))  ^ "}"
  | _                 -> assert false (* interpolation *)

and string_of_klass state klass =
  bind state (Class (klass, Assoc.empty)) ("c." ^ klass.k_name) (fun () ->
    "class " ^ (escape_as_literal klass.k_name) ^ " {\n" ^
      "  metaclass " ^ (string_of_klass state klass.k_metaclass) ^ "\n" ^
      (Option.map_default (fun klass ->
          "  objectclass " ^ (string_of_klass state klass) ^ "\n")
        "" klass.k_objectclass) ^
      (Option.map_default (fun klass ->
          "  ancestor " ^ (string_of_klass state klass) ^ "\n")
        "" klass.k_ancestor) ^
      (string_of_some (Assoc.is_empty klass.k_parameters) (fun () ->
        "  parameters { " ^
          (string_of_assoc "  " klass.k_parameters (string_of_tvar state)) ^
        " }\n")) ^
      (string_of_some (Assoc.is_empty klass.k_ivars) (fun () ->
        "  ivars {" ^
          (string_of_assoc "  " klass.k_ivars (string_of_ivar state)) ^
        "}\n")) ^
      (string_of_some (Assoc.is_empty klass.k_methods) (fun () ->
        "  methods {" ^
          (string_of_assoc "  " klass.k_methods (string_of_method state)) ^
        "}\n")) ^
      (string_of_some (klass.k_prepended = []) (fun () ->
        "  prepended [" ^
          (string_of_seq klass.k_prepended (string_of_mixin state)) ^ "]\n")) ^
      (string_of_some (klass.k_appended = []) (fun () ->
        "  appended ["  ^
          (string_of_seq klass.k_appended (string_of_mixin state)) ^ "]\n")) ^
    "}\n")

and string_of_mixin state mixin =
  bind state (Mixin (mixin, Assoc.empty)) ("m." ^ mixin.m_name) (fun () ->
    "mixin " ^ (escape_as_literal mixin.m_name) ^ " {\n" ^
      "  metaclass " ^ (string_of_klass state mixin.m_metaclass) ^ "\n" ^
      (string_of_some (Assoc.is_empty mixin.m_methods) (fun () ->
        "  methods {" ^
          (string_of_assoc "  " mixin.m_methods (string_of_method state)) ^
        "}\n")) ^
    "}\n")

and string_of_tvar state (tvar:tvar) =
  "tvar(" ^ (string_of_int (tvar :> int)) ^ ")"

and string_of_lambda_ty state lambda_ty =
  "lambda (" ^
    (string_of_ty state lambda_ty.l_ty_args) ^ ", " ^
    (string_of_ty state lambda_ty.l_ty_kwargs) ^ ") -> " ^
    (string_of_ty state lambda_ty.l_ty_result)

and string_of_ivar state ivar =
  let kind =
    match ivar.iv_kind with
    | Syntax.IVarImmutable   -> "immutable"
    | Syntax.IVarMutable     -> "mutable"
    | Syntax.IVarMetaMutable -> "meta_mutable"
  in
  (string_of_loc ivar.iv_location) ^ " " ^
    kind ^ " " ^
    (string_of_ty state ivar.iv_ty)

and string_of_method state meth =
  (if meth.im_dynamic then "dynamic " else "") ^
    (string_of_lambda state meth.im_body)

and string_of_lambda state lam =
  bind state (Lambda lam) "" (fun () ->
    "lambda " ^ (string_of_loc lam.l_location) ^ " {\n" ^
      "  local_env " ^ (string_of_local_env state lam.l_local_env) ^ "\n" ^
      "  type_env {" ^
        (string_of_table "  " lam.l_type_env (string_of_tvar state)) ^
      "}\n" ^
      "  const_env [" ^
        (string_of_seq lam.l_const_env (string_of_package state)) ^
      "]\n" ^
      "  type " ^ (string_of_lambda_ty state lam.l_ty) ^ "\n" ^
      "  args " ^ (Unicode.adopt_utf8s
          (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_formal_args lam.l_args))) ^ "\n" ^
      "  body " ^ (Unicode.adopt_utf8s
          (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_exprs lam.l_body))) ^ "\n" ^
    "}\n")

and string_of_local_env state lenv =
  bind state (Environment lenv) ("") (fun () ->
    "environment {\n" ^
      (Option.map_default (fun parent ->
          "  parent " ^ (string_of_local_env state parent) ^ "\n")
        "" lenv.e_parent) ^
      "  bindings " ^ (string_of_bindings state lenv.e_bindings) ^ "\n" ^
    "}\n")

and string_of_local_env_ty state ty =
  "{" ^ string_of_table "    " ty.e_ty_bindings (fun b ->
    (string_of_loc b.b_ty_location) ^ " " ^
    (match b.b_ty_kind with
    | Syntax.LVarImmutable -> "immutable"
    | Syntax.LVarMutable   -> "mutable") ^ " " ^
    (string_of_ty state b.b_ty)) ^
  "}" ^
    (match ty.e_ty_parent with
    | Some parent_ty -> " -> " ^ (string_of_local_env_ty state parent_ty)
    | None -> "")

and string_of_bindings state bindings =
  "{" ^
    (string_of_table "  " bindings (fun b ->
      (string_of_loc b.b_location) ^ " " ^
      (match b.b_kind with
      | Syntax.LVarImmutable -> "immutable"
      | Syntax.LVarMutable   -> "mutable") ^ " " ^
      (string_of_value state b.b_value))) ^
  "}"

and string_of_bindings_ty state bindings =
  "{" ^
    (string_of_table "  " bindings (fun b ->
      (string_of_loc b.b_ty_location) ^ " " ^
      (match b.b_ty_kind with
      | Syntax.LVarImmutable -> "immutable"
      | Syntax.LVarMutable   -> "mutable") ^ " " ^
      (string_of_value state b.b_ty))) ^
  "}"

and string_of_package state package =
  bind state (Package package) ("p." ^ package.p_name) (fun () ->
    "package " ^ (escape_as_literal package.p_name) ^ " {\n" ^
      "  metaclass " ^ (string_of_klass state package.p_metaclass) ^ "\n" ^
      (string_of_some (Table.is_empty package.p_constants) (fun () ->
        "  constants {" ^
          (string_of_table "  " package.p_constants (string_of_value state)) ^
        "}\n"
      )) ^
    "}\n")

and string_of_instance state (klass, sp) ivars =
  bind state (Instance ((klass, sp), ivars)) "" (fun () ->
    "instance " ^ (string_of_klass state klass) ^
      "{" ^ (string_of_assoc_inline sp (string_of_value state)) ^ "} {" ^
      (string_of_table "" ivars (string_of_value state)) ^
    "}\n")

let rec string_of_ssa_name state value =
  let print value =
    match value with
    | { ty     = Rt.NilTy      } -> "nil"
    | { opcode = Const value   } -> string_of_value state value
    | { opcode = Function func } -> "@" ^ (escape_as_ident value.id)
    | _ -> "%" ^ (escape_as_ident value.id)
  in
  let prefix () =
    if value.ty <> Rt.NilTy then
      "%" ^ (escape_as_ident value.id) ^ " = " ^ (string_of_ty state value.ty) ^ " "
    else ""
  in
  let instr opcode operands =
    (prefix ()) ^ opcode ^ " " ^ (String.concat ", " operands)
  in
  let group_instr opcode subj operands =
    (prefix ()) ^ opcode ^ " " ^ subj ^ ", [" ^ (String.concat ", " operands) ^ "]"
  in
  let call_instr callee operands =
    (prefix ()) ^ callee ^ " (" ^ (String.concat ", " (List.map print operands)) ^ ")"
  in
  match value.opcode with
  | Const value ->
    string_of_value state value
  | Argument ->
    "%" ^ (escape_as_ident value.id)
  | Function func ->
    let ret_ty =
      match value.ty with
      | FunctionTy(args,ret) -> ret
      | _ -> assert false
    in
    "@" ^ (escape_as_ident value.id) ^ " = function (" ^
      (string_of_seq func.arguments
        (fun arg -> (string_of_ty state arg.ty) ^ " " ^ (print arg))) ^
    ") -> " ^
      (string_of_ty state ret_ty) ^ " {\n" ^
      (String.concat "\n"
        (List.map (string_of_ssa_name state) func.basic_blocks)) ^
    "}\n"
  | BasicBlock block ->
    let preds   = List.map print (predecessors value) in
    let preds   = if preds = [] then
                    if value == (func_entry (block_parent value)) then ""
                    else " ; No predecessors!"
                  else " ; preds = " ^ (String.concat ", " preds) in
    let ident   = (escape_as_ident value.id) ^ ":" in
    let header  = ident ^
      (String.make (50 - (String.length ident)) (Char.of_string " ")) ^
      preds ^ "\n"
    in
    header ^
      (String.concat ""
        (List.map
          (fun v -> "  " ^ (string_of_ssa_name state v) ^ "\n")
          block.instructions))
  (* Instructions *)
  | InvalidInstr ->
    instr "$invalid" []
  | JumpInstr name ->
    instr "jump" [print name]
  | JumpIfInstr (cond, if_true, if_false) ->
    instr "jump_if" [print cond; print if_true; print if_false]
  | ReturnInstr name ->
    instr "return" [print name]
  | PhiInstr operands ->
    (prefix ()) ^ "phi [" ^ (String.concat ", " (List.map (fun (block, value) ->
                  (print block) ^ " => " ^ (print value)) operands)) ^ "]"
  | FrameInstr (parent) ->
    instr "frame" [print parent]
  | LVarLoadInstr (env, var) ->
    instr "lvar_load" [print env; escape_as_literal var]
  | LVarStoreInstr (env, var, value) ->
    instr "lvar_store" [print env; escape_as_literal var; print value]
  | IVarLoadInstr (obj, var) ->
    instr "ivar_load" [print obj; escape_as_literal var]
  | IVarStoreInstr (obj, var, value) ->
    instr "ivar_store" [print obj; escape_as_literal var; print value]
  | CallInstr (func, operands) ->
    call_instr ("call " ^ (print func)) operands
  | ClosureInstr (func, env) ->
    instr "closure" [print func; print env]
  | ResolveInstr (obj, meth) ->
    instr "resolve" [print obj; print meth]
  | SpecializeInstr (cls, operands) ->
    group_instr "specialize" (print cls) (Assoc.map_list
          (fun param value -> (escape_as_literal param) ^ " => " ^ (print value)) operands)
  | TupleExtendInstr (tup, elems) ->
    group_instr "tuple_extend" (print tup) (List.map print elems)
  | TupleConcatInstr (tup, tup') ->
    instr "tuple_concat" [print tup; print tup']
  | RecordExtendInstr (re, elems) ->
    group_instr "record_extend" (print re) (List.map
          (fun (key, value) -> (print key) ^ " => " ^ (print value)) elems)
  | RecordConcatInstr (re, re') ->
    instr "record_concat" [print re; print re']
  | PrimitiveInstr (name, operands) ->
    call_instr ("primitive " ^ (escape_as_literal name)) operands

let string_of_roots state roots =
  List.iter (fun klass -> ignore (string_of_klass state klass)) [
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
  ignore (string_of_package state roots.pToplevel)

let string_of_capsule state capsule =
  let funcs =
    if !ordered then
      List.sort (fun a b -> compare a.id b.id) capsule.functions
    else
      capsule.functions
  in
  List.iter (fun funcn ->
      state.image <- state.image ^ (string_of_ssa_name state funcn) ^ "\n")
    funcs;

  iter_overloads capsule ~f:(fun funcn funcn' ->
    state.image <- state.image ^
      "map function @" ^ (escape_as_ident funcn.id) ^
               " => @" ^ (escape_as_ident funcn'.id) ^ "\n");

  iter_lambdas capsule ~f:(fun lambda funcn ->
    let lambda = string_of_lambda state lambda in
    state.image <- state.image ^
      "map lambda " ^ lambda ^
            " => @" ^ (escape_as_ident funcn.id) ^ "\n")

let print_name name =
  let state = create_state (Symtab.create ()) in
  prerr_endline (string_of_ssa_name state name)

let string_of ?(omit_roots=false) roots capsule =
  let state = create_state capsule.Ssa.c_symtab in
  if not omit_roots then
    ignore (string_of_roots state roots);
  string_of_capsule state capsule;
  state.image <- state.image ^
    "map tvar = " ^ (string_of_int roots.last_tvar) ^ "\n";
  state.image
