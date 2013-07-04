open Sexplib.Std

type typevar = int
with sexp_of

type value =
(* Primitives *)
| Tvar          of typevar
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
| Environment   of environment
| EnvironmentTy of environment_ty
| Closure       of closure
| ClosureTy     of closure_ty
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
and environment_ty = {
  e_parent_ty     : environment_ty option;
  e_bindings_ty   : binding_ty Table.t;
}
and binding = {
  b_location      : Location.t;
  b_is_mutable    : bool;
  b_value         : value;
}
and environment = {
  e_parent        : environment option;
  e_bindings      : binding Table.t;
}
and closure_ty = {
  l_args_ty   : value;
  l_kwargs_ty : value;
  l_return_ty : value;
}
and closure = {
  l_ty        : closure_ty;
  l_env       : environment;
  l_code      : Syntax.expr;
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
  im_body     : closure;
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
  | Truth | Lies -> BooleanTy
  | Nil          -> NilTy

  | Int(_)       -> IntTy
  | Symbol(_)    -> SymbolTy
  | Tuple(xs)    -> TupleTy(List.map typeof xs)
  | Record(xs)   -> RecordTy(Table.map (fun v -> typeof v) xs)

  (* | Environment(e)
  -> EnvironmentTy(None, Table.map (fun (_, m) -> m) e.e_bindings)
 *)
  | Closure(c)   -> ClosureTy(c.l_ty)

  | Instance(k,_) -> Class(k)
  | _ -> assert false

let string_of_value value =
   (Sexplib.Sexp.to_string_hum (sexp_of_value value))

let inspect value =
  (string_of_value value) ^ " : " ^ (string_of_value (typeof value))

(* Exceptions *)

let exc_fail message loc hilights =
  raise (Exc {
    ex_message    = message;
    ex_location   = loc;
    ex_highlights = hilights;
  })

let exc_type expected obj loc =
  exc_fail (expected ^ " expected; " ^ (inspect obj) ^ " found") loc []

(* Environments *)

exception EnvUnbound
exception EnvAlreadyBound of binding
exception EnvImmutable    of binding

let env_create () =
  { e_parent   = None;
    e_bindings = Table.create () }

let env_bind env name ~loc ~is_mutable ~value =
  match Table.get env.e_bindings name with
  | Some(b)
  -> raise (EnvAlreadyBound b)
  | None
  -> Table.set env.e_bindings name {
    b_location   = loc;
    b_is_mutable = is_mutable;
    b_value      = value;
  }

let rec env_mutate env name ~value =
  match Table.get env.e_bindings name with
  | Some({ b_is_mutable = false } as b)
  -> raise (EnvImmutable b)
  | Some b
  -> Table.set env.e_bindings name {
    b_location   = b.b_location;
    b_is_mutable = true;
    b_value      = value;
  }
  | None
  -> match env.e_parent with
     | Some parent -> env_mutate parent name value
     | None -> raise EnvUnbound

let rec env_lookup env name =
  match Table.get env.e_bindings name with
  | Some({ b_value = value }) -> value
  | None
  -> match env.e_parent with
     | Some parent -> env_lookup parent name
     | None -> raise EnvUnbound

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

and eval_pattern env pat value =
  let bind name ~is_mutable ~value ~loc =
    try
      env_bind env name ~is_mutable ~value ~loc
    with EnvAlreadyBound({ b_location = bound_loc }) ->
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

and eval_assign env lhs value =
  match lhs with
  | Syntax.Var((loc,_),name)
  -> (try
        env_mutate env name ~value:value
      with
      | EnvImmutable({ b_location = bound_loc }) ->
        exc_fail ("Name " ^ name ^ " is bound as immutable") loc [bound_loc]
      | EnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") loc [])
  | _ -> assert false

and eval env expr =
  match expr with
  | Syntax.Truth(_) -> Truth
  | Syntax.Lies(_)  -> Lies
  | Syntax.Int(_,x) -> Int(x)
  | Syntax.Sym(_,x) -> Symbol(x)
  | Syntax.Tuple(_,xs)
  -> List.fold_left concat_tuple (Tuple []) (List.map (eval_tuple env) xs)
  | Syntax.Record(_,xs)
  -> List.fold_left concat_record (Record (Table.create ())) (List.map (eval_record env) xs)
  | Syntax.Let(_,pat,_ty,expr)
  -> (let value = eval env expr in
        eval_pattern env pat (eval env expr);
        value)
  | Syntax.Var(loc,name)
  -> (try
        env_lookup env name
      with EnvUnbound ->
        exc_fail ("Name " ^ name ^ " is not bound") (Syntax.loc expr) [])
  | Syntax.Assign(_,lhs,rhs)
  -> (let value = eval env rhs in
        eval_assign env lhs value;
        value)
  | _
  -> failwith ("cannot eval " ^ Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr));
