open Sexplib.Std

type typevar = int
with sexp_of

type value =
(* Primitives *)
| Tvar          of typevar
| TvarTy
| Nil
| NilTy
| Truth
| Lies
| BooleanTy
| Int           of int
| IntTy
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
and type_env =      typevar Table.t
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
  p_name      : string;
  p_metaclass : klass;
  p_constants : value Table.t;
}
and klass = {
  k_name      : string;
  k_metaclass : klass;
  k_ancestor  : klass   option;
  k_ivars     : ivar    Table.t;
  k_methods   : imethod Table.t;
  k_prepended : mixin   list;
  k_appended  : mixin   list;
}
and mixin = {
  m_name      : string;
  m_metaclass : klass;
  m_methods   : imethod Table.t;
}
and imethod = {
  im_body     : lambda;
  im_dynamic  : bool;
}
and ivar_kind =
| IvarImmutable
| IvarMutable
| IvarMetaMutable
and ivar = {
  iv_ty       : value;
  iv_kind     : ivar_kind;
}
and exc = {
  ex_message    : string;
  ex_location   : int * int;
  ex_highlights : (int * int) list;
}
with sexp_of

exception Exc of exc
with sexp

let lastvar = ref 0
let genvar () =
  incr lastvar;
  Tvar (!lastvar)

let rec typeof value =
  match value with
  | Truth | Lies  -> BooleanTy
  | Nil           -> NilTy

  | Tvar(_)       -> TvarTy
  | Int(_)        -> IntTy
  | Symbol(_)     -> SymbolTy
  | Tuple(xs)     -> TupleTy(List.map typeof xs)
  | Record(xs)    -> RecordTy(Table.map (fun v -> typeof v) xs)

  | Lambda(c)     -> c.l_ty

  | Instance(k,_) -> Class(k)
  | _ -> failwith ("cannot typeof " ^ (Sexplib.Sexp.to_string_hum (sexp_of_value value)))

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
    | Tuple(xs)    -> "[" ^ (String.concat ", " (List.map inspect_value xs)) ^ "]"
    | Record(xs)   -> "{" ^ (String.concat ", " (Table.map_list
                                (fun k v -> k ^ ": " ^ (inspect_value v)) xs)) ^ "}"
    | Lambda(lm)   -> "#<Lambda " ^ Location.at(Syntax.loc lm.l_code) ^ ">"
    | _ -> (string_of_value value))

let rec inspect_type_pair name ty =
  name ^ ": " ^ (inspect_type ty)

and inspect_type ty =
  inspect_literal_or ty (fun x ->
    match ty with
    | TvarTy       -> "TypeVariable"
    | BooleanTy    -> "Boolean"
    | NilTy        -> "Nil"
    | IntTy        -> "Integer"
    | SymbolTy     -> "Symbol"
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
    | Tvar(tv)     -> "\\" ^ (string_of_int tv)
    | _            -> "((" ^ (inspect_value ty) ^ "))")

let inspect value =
  (inspect_value value) ^ " : " ^ (inspect_type (typeof value))

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

let lenv_create () =
  { e_parent   = None;
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

let tenv_create () : typevar Table.t =
  Table.create []

(* Eval helper routines *)

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

(* E V A L *)

let rec eval_tuple env elem =
  match elem with
  | Syntax.TupleElem(_,expr)
  -> Tuple [eval env expr]

  | Syntax.TupleSplice(_,expr)
  -> (match (eval env expr) with
      | Tuple(_) as t -> t
      | o -> exc_type "Tuple" o (Syntax.loc expr))

and eval_record env elem =
  match elem with
  | Syntax.RecordElem(_,k,v)
  -> Record (Table.pair k (eval env v))

  | Syntax.RecordSplice(_,expr)
  -> (match (eval env expr) with
     | Record(_) as r -> r
     | o -> exc_type "Record" o (Syntax.loc expr))

  | Syntax.RecordPair(_,k,v)
  -> (match (eval env k) with
     | Symbol(s) -> Record (Table.pair s (eval env v))
     | o -> exc_type "Symbol" o (Syntax.loc k))

and eval_pattern ((lenv, tenv) as env) pat value =
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

and eval_assign (lenv, tenv) lhs value =
  match lhs with
  | Syntax.Var((loc,_),name)
  -> (try
        lenv_mutate lenv name ~value:value
      with
      | LEnvImmutable({ b_location = bound_loc }) ->
        exc_fail ("Name " ^ name ^ " is bound as immutable") loc [bound_loc]
      | LEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") loc [])
  | _ -> assert false

and eval_type ((lenv, tenv) as env) expr =
  let as_type expr =
    let ty = eval_type env expr in
      match ty with
      | Tvar(_) | TvarTy | NilTy | BooleanTy
      | IntTy | SymbolTy
      | TupleTy(_) | RecordTy(_) | LambdaTy(_)
      -> ty
      | _ -> exc_type "type" ty (Syntax.ty_loc expr)
  in
  match expr with
  | Syntax.TypeVar(_,n)
  -> (* TODO TODO TODO *) genvar ()
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
  | Syntax.TypeConstr(_,name,args)
  -> (match name with
      | "Nil"     -> NilTy
      | "True" | "False" -> BooleanTy
      | "Integer" -> IntTy
      | "Symbol"  -> SymbolTy
      | _ -> failwith ("cannot type eval " ^ name ^ ": no general type constr support"))
  | Syntax.TypeSplice(_,expr)
  -> eval env expr

and eval ((lenv, tenv) as env) expr =
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
  | Syntax.Let(_,pat,_ty,expr)
  -> (let value = eval env expr in
        eval_pattern env pat value;
        value)
  | Syntax.Var(loc,name)
  -> (try
        lenv_lookup lenv name
      with LEnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") (Syntax.loc expr) [])
  | Syntax.Self(loc)
  -> (try
        lenv_lookup lenv "self"
      with LEnvUnbound ->
        exc_fail ("self is not bound. (self does not exist outside of methods)")
                 (Syntax.loc expr) [])
  | Syntax.Assign(_,lhs,rhs)
  -> (let value = eval env rhs in
        eval_assign env lhs value;
        value)
  | Syntax.Lambda(_,_,Some ty_expr,_)
  -> (let ty = eval_type env ty_expr in
        match ty with
        | LambdaTy(_) | Tvar(_)
        -> Lambda { l_ty        = ty;
                    l_local_env = lenv;
                    l_type_env  = tenv;
                    l_code      = expr }
        | _
        -> exc_type "closure type" ty (Syntax.ty_loc ty_expr))
  | Syntax.Lambda(_,_,None,_)
  -> Lambda { l_ty        = genvar ();
              l_local_env = lenv;
              l_type_env  = tenv;
              l_code      = expr }
  | Syntax.Type(_,ty_expr)
  -> eval_type env ty_expr
  | _
  -> failwith ("cannot eval " ^ Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr));
