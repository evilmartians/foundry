open Sexplib.Std

type tvar = int
with sexp_of

type value =
(* Primitives *)
| Tvar          of tvar
| TvarTy
| Nil
| NilTy
| Truth
| Lies
| BooleanTy
| Int           of int
| IntegerTy
| Symbol        of string
| SymbolTy
(* Product types *)
| Tuple         of value list
| TupleTy       of value list
| Record        of value Table.t
| RecordTy      of value Table.t
(* Function type *)
| Environment   of local_env
| EnvironmentTy of local_env_ty
| Lambda        of lambda
| LambdaTy      of lambda_ty
(* Packages *)
| Package       of package
(* User-defined types *)
| Class         of klass specialized
| Mixin         of mixin specialized
| Instance      of klass specialized * value Table.t
and binding_ty = {
  b_location_ty   : Location.t;
  b_is_mutable_ty : bool;
  b_value_ty      : value;
}
and local_env_ty = {
  e_parent_ty     : local_env_ty option;
  e_bindings_ty   : binding_ty Table.t;
}
and binding = {
  b_location      : Location.t;
  b_is_mutable    : bool;
  b_value         : value;
}
and local_env = {
  e_parent        : local_env option;
  e_bindings      : binding Table.t;
}
and type_env =      tvar Table.t
and const_env =     package list ref
and lambda = {
  l_ty            : value;
  l_local_env     : local_env;
  l_type_env      : type_env;
  l_code          : Syntax.expr;
}
and lambda_ty = {
  l_args_ty       : value;
  l_kwargs_ty     : value;
  l_return_ty     : value;
}
and 'a specialized = 'a * value Table.t
and package = {
  p_name          : string;
  p_metaclass     : klass;
  p_constants     : value Table.t;
}
and klass = {
  k_name          : string;
  k_metaclass     : klass;
  k_ancestor      : klass   option;
  k_tvars         : tvar    Table.t;
  k_ivars         : ivar    Table.t;
  k_methods       : imethod Table.t;
  mutable k_prepended : mixin list;
  mutable k_appended  : mixin list;
}
and mixin = {
  m_name          : string;
  m_metaclass     : klass;
  m_methods       : imethod Table.t;
}
and imethod = {
  im_body         : lambda;
  im_dynamic      : bool;
}
and ivar = {
  iv_location     : Location.t;
  iv_kind         : Syntax.ivar_kind;
  iv_ty           : value;
}
and exc = {
  ex_message      : string;
  ex_location     : int * int;
  ex_highlights   : (int * int) list;
}
with sexp_of

exception Exc of exc
with sexp

(* Class tooling & default virtual image *)

let rec kClass =
  { k_name      = "Class";
    k_ancestor  = None;
    k_metaclass = kmetaClass;
    k_tvars     = Table.create [];
    k_ivars     = Table.create [];
    k_methods   = Table.create [];
    k_prepended = [];
    k_appended  = []; }
and kmetaClass =
  { k_name      = "meta:Class";
    k_ancestor  = Some kClass;
    k_metaclass = kClass;
    k_tvars     = Table.create [];
    k_ivars     = Table.create [];
    k_methods   = Table.create [];
    k_prepended = [];
    k_appended  = []; }

let new_metaclass ?ancestor name =
  { k_name      = "meta:" ^ name;
    k_ancestor  = ancestor;
    k_metaclass = kClass;
    k_tvars     = Table.create [];
    k_ivars     = Table.create [];
    k_methods   = Table.create [];
    k_prepended = [];
    k_appended  = []; }

let new_class ?ancestor name =
  let meta_ancestor =
    match ancestor with
    | Some klass -> klass.k_ancestor
    | None -> None
  in
  { k_name      = name;
    k_metaclass = (new_metaclass ?ancestor:meta_ancestor name);
    k_ancestor  = ancestor;
    k_tvars     = Table.create [];
    k_ivars     = Table.create [];
    k_methods   = Table.create [];
    k_prepended = [];
    k_appended  = []; }

let kTypeVariable = new_class "TypeVariable"

let kNil     = new_class "Nil"
let kBoolean = new_class "Boolean"
let kInteger = new_class "Integer"
let kSymbol  = new_class "Symbol"

let kMixin   = new_class "Mixin"
let kPackage = new_class "Package"

let new_package name =
  { p_name      = name;
    p_metaclass = new_metaclass ~ancestor:kPackage name;
    p_constants = Table.create [] }

let pToplevel = new_package "#<toplevel>"

let () =
  Table.fill pToplevel.p_constants [
    ("Nil",     NilTy);
    ("Boolean", BooleanTy);
    ("Integer", IntegerTy);
    ("Symbol",  SymbolTy)
  ]

(* Types and type variables *)

let lastvar = ref 0
let genvar () : tvar =
  incr lastvar;
  !lastvar

let rec typeof value =
  match value with
  | Truth | Lies  -> BooleanTy
  | Nil           -> NilTy

  | Tvar(_)       -> TvarTy
  | Int(_)        -> IntegerTy
  | Symbol(_)     -> SymbolTy
  | Tuple(xs)     -> TupleTy(List.map typeof xs)
  | Record(xs)    -> RecordTy(Table.map (fun v -> typeof v) xs)

  | Lambda(c)     -> c.l_ty

  | Package(_)    -> Class(kPackage, Table.create [])
  | Class(k,_)    -> Class(kClass, Table.create [])
  | Instance(k,_) -> Class(k)
  | _ -> failwith ("cannot typeof " ^ (Sexplib.Sexp.to_string_hum (sexp_of_value value)))

(* Inspecting types and values *)

let string_of_value value =
   (Sexplib.Sexp.to_string_hum (sexp_of_value value))

let inspect_literal_or value f =
  match value with
  | Truth     -> "true"
  | Lies      -> "false"
  | Nil       -> "nil"
  | Int(n)    -> string_of_int n
  | Symbol(s) -> ":" ^ s
  | _         -> f value

let rec inspect_value value =
  inspect_literal_or value (fun x ->
    match value with
    | Tuple(xs)
    -> "[" ^ (String.concat ", " (List.map inspect_value xs)) ^ "]"
    | Record(xs)
    -> "{" ^ (String.concat ", " (Table.map_list
                (fun k v -> k ^ ": " ^ (inspect_value v)) xs)) ^ "}"
    | Lambda(lm)
    -> "#<Lambda " ^ Location.at(Syntax.loc lm.l_code) ^ ">"
    | TvarTy     -> "TypeVariable"
    | BooleanTy  -> "Boolean"
    | NilTy      -> "Nil"
    | IntegerTy  -> "Integer"
    | SymbolTy   -> "Symbol"
    | TupleTy(_) | RecordTy(_) | LambdaTy(_)
    -> "type " ^ (inspect_type value)
    | Class(k,_) -> k.k_name
    | Package(p) -> p.p_name
    | _ -> (string_of_value value))

and inspect_type_pair name ty =
  name ^ ": " ^ (inspect_type ty)

and inspect_type ty =
  inspect_literal_or ty (fun x ->
    match ty with
    | TvarTy       -> "TypeVariable"
    | BooleanTy    -> "Boolean"
    | NilTy        -> "Nil"
    | IntegerTy    -> "Integer"
    | SymbolTy     -> "Symbol"
    | Tvar(tv)     -> "\\" ^ (string_of_int tv)
    | TupleTy(xs)  -> "[" ^ (String.concat ", " (List.map inspect_type xs)) ^ "]"
    | RecordTy(xs) -> "{" ^ (String.concat ", " (Table.map_list inspect_type_pair xs)) ^ "}"
    | LambdaTy(lm)
    -> (let args_ty =
          match lm.l_args_ty with
          | TupleTy(xs) -> List.map inspect_type xs
          | o -> ["*" ^ (inspect_type o)]
        in let kwargs_ty =
          match lm.l_kwargs_ty with
          | RecordTy(xs) -> Table.map_list inspect_type_pair xs
          | o -> ["**" ^ (inspect_type o)]
        in "(" ^ (String.concat ", " (args_ty @ kwargs_ty)) ^
           ") -> " ^ (inspect_type lm.l_return_ty))
    | Class(k,_)   -> k.k_name
    | _            -> "((" ^ (inspect_value ty) ^ "))")

let inspect value =
  let ty =
    try  (inspect_type (typeof value))
    with Failure(_) -> "#<untypable value>"
  in (inspect_value value) ^ " : " ^ ty

(* Exceptions *)

let exc_fail message loc hilights =
  raise (Exc {
    ex_message    = message;
    ex_location   = loc;
    ex_highlights = hilights;
  })

let exc_type expected obj loc =
  exc_fail (expected ^ " expected; " ^ (inspect obj) ^ " found") loc []

(* Local environment *)

exception LEnvUnbound
exception LEnvAlreadyBound of binding
exception LEnvImmutable    of binding

let lenv_create parent : local_env =
  { e_parent   = parent;
    e_bindings = Table.create [] }

let lenv_bind env name ~loc ~is_mutable ~value =
  match Table.get env.e_bindings name with
  | Some(b)
  -> raise (LEnvAlreadyBound b)
  | None
  -> Table.set env.e_bindings name {
    b_location   = loc;
    b_is_mutable = is_mutable;
    b_value      = value;
  }

let rec lenv_mutate env name ~value =
  match Table.get env.e_bindings name with
  | Some({ b_is_mutable = false } as b)
  -> raise (LEnvImmutable b)
  | Some b
  -> Table.set env.e_bindings name {
    b_location   = b.b_location;
    b_is_mutable = true;
    b_value      = value;
  }
  | None
  -> match env.e_parent with
     | Some parent -> lenv_mutate parent name value
     | None -> raise LEnvUnbound

let rec lenv_lookup env name =
  match Table.get env.e_bindings name with
  | Some({ b_value = value }) -> value
  | None
  -> match env.e_parent with
     | Some parent -> lenv_lookup parent name
     | None -> raise LEnvUnbound

(* Type environment *)

let tenv_create () : type_env =
  Table.create []

let tenv_fork env =
  Table.copy env

let tenv_resolve env name =
  match Table.get env name with
  | Some tvar -> tvar
  | None ->
    let tvar = genvar () in
      Table.set env name tvar;
      tvar

(* Constant environment *)

exception CEnvUnbound
exception CEnvAlreadyBound of value

let cenv_create () : const_env =
  ref [pToplevel]

let cenv_fork env =
  ref !env

let cenv_extend env pkg =
  env := pkg :: !env

let cenv_bind env name value =
  let pkg = List.hd !env in
    match Table.get pkg.p_constants name with
    | Some value -> raise (CEnvAlreadyBound value)
    | None -> Table.set pkg.p_constants name value

let cenv_peek env name =
  let pkg = List.hd !env in
    Table.get pkg.p_constants name

let cenv_lookup env name =
  let rec lookup lst =
    match lst with
    | pkg :: rest
    -> (match Table.get pkg.p_constants name with
        | Some value -> value
        | None -> lookup rest)
    | []
    -> raise CEnvUnbound

  in lookup !env

(* Eval helper routines *)

let env_create () =
  let lenv = lenv_create None
  in lenv_bind lenv "self" ~value:(Package pToplevel) ~is_mutable:false ~loc:(0,0);
     lenv, tenv_create (), cenv_create ()

let concat_tuple lhs rhs =
  match lhs, rhs with
  | Tuple(l), Tuple(r)
  -> Tuple(l @ r)
  | _ -> assert false

let concat_record lhs rhs =
  match lhs, rhs with
  | Record(l), Record(r)
  -> Record(Table.join l r)
  | _ -> assert false

let check_class lenv loc =
  match lenv_lookup lenv "self" with
  | Class (klass,_) -> klass
  | value -> exc_type "class" value loc

(* E V A L *)

let rec eval_tuple env elem =
  match elem with
  | Syntax.TupleElem(_,expr)
  -> Tuple [eval_expr env expr]

  | Syntax.TupleSplice(_,expr)
  -> (match (eval_expr env expr) with
      | Tuple(_) as t -> t
      | o -> exc_type "Tuple" o (Syntax.loc expr))

and eval_record env elem =
  match elem with
  | Syntax.RecordElem(_,k,v)
  -> Record (Table.pair k (eval_expr env v))

  | Syntax.RecordSplice(_,expr)
  -> (match (eval_expr env expr) with
     | Record(_) as r -> r
     | o -> exc_type "Record" o (Syntax.loc expr))

  | Syntax.RecordPair(_,k,v)
  -> (match (eval_expr env k) with
     | Symbol(s) -> Record (Table.pair s (eval_expr env v))
     | o -> exc_type "Symbol" o (Syntax.loc k))

and eval_pattern ((lenv, tenv, cenv) as env) pat value =
  let bind name ~is_mutable ~value ~loc =
    try
      lenv_bind lenv name ~is_mutable ~value ~loc
    with LEnvAlreadyBound({ b_location = bound_loc }) ->
      exc_fail ("Name " ^ name ^ " is already bound") loc [bound_loc]
  in

  match pat with
  | Syntax.PatImmutable((loc,_),name)
  -> bind name ~is_mutable:false ~value ~loc
  | Syntax.PatMutable((loc,_),name)
  -> bind name ~is_mutable:true ~value ~loc
  | Syntax.PatTuple((loc,_),pats)
  -> (match value with
      | Tuple(xs)
      -> (if List.length(xs) = List.length(pats) then
            List.iter2 (eval_pattern env) pats xs
          else
            exc_fail ("Tuple " ^ (inspect value) ^ " of length " ^
                      (string_of_int (List.length xs)) ^
                      " does not match pattern of length " ^
                      (string_of_int (List.length pats))) loc [])
      | o -> exc_type "Tuple" o (Syntax.pat_loc pat))
  | _ -> assert false

and eval_assign (lenv, tenv, cenv) lhs value =
  match lhs with
  | Syntax.Var((loc,_),name)
  -> (try
        lenv_mutate lenv name ~value:value
      with
      | LEnvImmutable({ b_location = bound_loc }) ->
        exc_fail ("Name " ^ name ^ " is bound as immutable") loc [bound_loc]
      | LEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") loc [])
  | Syntax.Const((loc,_),name)
  -> (try
        cenv_bind cenv name value
      with CEnvAlreadyBound(value) ->
        exc_fail ("Name " ^ name ^ " is already bound to " ^ (inspect value)) loc [])
  | _ -> assert false

and eval_type ((lenv, tenv, cenv) as env) expr =
  let as_type expr =
    let ty = eval_type env expr in
      match ty with
      | Tvar(_) | TvarTy | NilTy | BooleanTy
      | IntegerTy | SymbolTy
      | TupleTy(_) | RecordTy(_) | LambdaTy(_)
      | Class(_,_)
      -> ty
      | _ -> exc_type "type" ty (Syntax.ty_loc expr)
  in
  match expr with
  | Syntax.TypeVar(_,name)
  -> Tvar (tenv_resolve tenv name)
  | Syntax.TypeTuple(_,xs)
  -> TupleTy (List.map as_type xs)
  | Syntax.TypeRecord(_,xs)
  -> RecordTy (Table.create (List.map (fun (_,k,v) -> k, as_type v) xs))
  | Syntax.TypeFunction(_,all_args,ret)
  -> (let args, kwargs =
        List.fold_left (fun (args, kwargs) arg ->
          match arg with
          | Syntax.TypeArg(_,ty)     -> ((as_type ty) :: args), kwargs
          | Syntax.TypeArgKw(_,n,ty) -> args, (n, (as_type ty)) :: kwargs)
        ([], []) all_args
      in LambdaTy {
        l_args_ty   = TupleTy  args;
        l_kwargs_ty = RecordTy (Table.create kwargs);
        l_return_ty = as_type ret;
      })
  | Syntax.TypeConstr((loc,_),name,args)
  -> (match name with
      | "Tuple"
      -> exc_fail "Use [...] syntax to construct tuple types" loc []
      | "Record"
      -> exc_fail "Use {...} syntax to construct record types" loc []
      | "Lambda"
      -> exc_fail "Use (...) -> ... syntax to construct lambda types" loc []
      | _
      -> (try
            match cenv_lookup cenv name with
            | (NilTy | BooleanTy | IntegerTy | SymbolTy | TvarTy) as ty
            -> (match args with
                | [] -> ty
                | _  -> exc_fail ("Type " ^ name ^ " is not parametric") loc [])
            | Class(klass,specz)
            -> (let new_specz =
                  Table.create (List.map
                    (fun ((loc,_), name, expr) ->
                      if Table.exists klass.k_tvars name then
                        name, eval_type env expr
                      else
                        exc_fail ("Type " ^ klass.k_name ^
                                  " is not parametric by " ^ name) loc []) args)
                in Class(klass, Table.join specz new_specz))
            | value
            -> exc_fail ("Name " ^ name ^ " is bound to " ^ (inspect value) ^
                         " which is not a type") loc []
          with CEnvUnbound ->
            exc_fail ("Name " ^ name ^ " is unbound") loc []))
  | Syntax.TypeSplice(_,expr)
  -> eval_expr env expr

and eval_closure_ty (lenv, tenv, cenv) expr =
  Option.map_default (fun ty_expr ->
    let tenv = tenv_fork tenv in
      let ty = eval_type (lenv, tenv, cenv) ty_expr in
        match ty with
        | LambdaTy(_) | Tvar(_)
        -> tenv, ty
        | _
        -> exc_type "closure type" ty (Syntax.ty_loc ty_expr))
    (tenv, Tvar (genvar ()))
    expr

and eval_expr ((lenv, tenv, cenv) as env) expr =
  match expr with
  | Syntax.Nil(_)   -> Nil
  | Syntax.Truth(_) -> Truth
  | Syntax.Lies(_)  -> Lies
  | Syntax.Int(_,x) -> Int(x)
  | Syntax.Sym(_,x) -> Symbol(x)
  | Syntax.Tuple(_,xs)
  -> List.fold_left concat_tuple
        (Tuple []) (List.map (eval_tuple env) xs)
  | Syntax.Record(_,xs)
  -> List.fold_left concat_record
        (Record (Table.create [])) (List.map (eval_record env) xs)
  | Syntax.Type(_,ty_expr)
  -> eval_type (lenv, (tenv_fork tenv), cenv) ty_expr
  | Syntax.Let(_,pat,_ty,expr)
  -> (let value = eval_expr env expr in
        eval_pattern env pat value;
        value)
  | Syntax.Var(loc,name)
  -> (try
        lenv_lookup lenv name
      with LEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") (Syntax.loc expr) [])
  | Syntax.Self(loc)
  -> lenv_lookup lenv "self"
  | Syntax.Const(loc,name)
  -> (try
        cenv_lookup cenv name
      with CEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") (Syntax.loc expr) [])
  | Syntax.Assign(_,lhs,rhs)
  -> (let value = eval_expr env rhs in
        eval_assign env lhs value;
        value)
  | Syntax.Lambda(_,_,ty_expr,_)
  -> (let tenv, ty = eval_closure_ty env ty_expr in
        Lambda { l_ty        = ty;
                 l_local_env = lenv;
                 l_type_env  = tenv;
                 l_code      = expr })
  | Syntax.Class((loc,_),name,ancestor,body)
  -> (let ancestor, specz =
        (* Extract ancestor class object and ancestor specialization table *)
        match ancestor with
        | Some expr
        -> (match eval_expr env expr with
            | Class (klass,specz) -> Some klass, Some specz
            | value -> exc_type "inheritable class" value (Syntax.loc expr))
        | None
        -> None, None
      in
      (* Check if we should extend existing class, or create a new one
         and bind it *)
      let klass =
        match cenv_peek cenv name with
        (* There's an existing one, and it is compatible *)
        | Some (Class (klass,_) as value) when klass.k_ancestor = ancestor
        -> value
        | Some (Class (klass,_) as value) when ancestor = None
        -> value
        (* There's an existing one, and it is not compatible with
           the present definition *)
        | Some (Class (klass,_))
        -> (let inspect_ancestor ancestor =
              match ancestor with
              | Some klass -> "has ancestor " ^ klass.k_name
              | None -> "does not have an ancestor"
            in
              exc_fail ("Cannot reopen " ^ name ^ ": it " ^
                        (inspect_ancestor klass.k_ancestor) ^
                        ", and the definition " ^
                        (inspect_ancestor ancestor)) loc []) (* TODO loc *)
        (* Not a class *)
        | Some value
        -> exc_fail ("Cannot reopen " ^ name ^ ": it is bound to " ^
                     (inspect value) ^ ", which is not a class") loc []
        (* No class present, create one and inherit specializations from
           its ancestor *)
        | None
        -> (let specz = Option.map_default Table.copy (Table.create []) specz
            in let value = Class (new_class ?ancestor name, specz)
               in cenv_bind cenv name value; value)
      in
      (* Evaluate class body in a context where self is bound to the class *)
      let lenv = lenv_create (Some lenv) in
        lenv_bind lenv "self" ~value:klass ~is_mutable:false ~loc:loc;
          eval (lenv, tenv, cenv) body)
  | Syntax.DefMethod((loc,_),name,args,ty_expr,body)
  -> Nil
  | Syntax.DefIVar((loc,_),name,kind,ty_expr)
  -> (let klass = check_class lenv loc in
      match Table.get klass.k_ivars name with
      | Some ivar
      -> exc_fail ("Cannot define @" ^ name ^ " on " ^
                   (inspect_value (lenv_lookup lenv "self")) ^
                   ": it is already defined with type " ^
                   (inspect_type ivar.iv_ty)) loc [ivar.iv_location]
      | None
      -> (Table.set klass.k_ivars name {
            iv_location = loc;
            iv_ty       = eval_type env ty_expr;
            iv_kind     = kind;
          });
      Nil)
  | _
  -> failwith ("cannot eval " ^ Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr));

and eval env exprs =
  Option.default Nil
    (List.fold_left (fun _ expr -> Some (eval_expr env expr)) None exprs)
