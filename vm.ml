open Sexplib.Std

type typevar = int
with sexp_of

module Table = struct
  type 'a t = (string, 'a) Hashtbl.t
  with sexp_of

  let newtable arg f =
    let table = Hashtbl.create (Hashtbl.length arg) in
      f table; table

  let empty () =
    Hashtbl.create 1

  let pair k v =
    let table = Hashtbl.create 1 in
      Hashtbl.add table k v;
      table

  let map f arg =
    newtable arg (fun table ->
      Hashtbl.iter (fun k v -> Hashtbl.add table k (f v)) arg)

  let join l r =
    newtable l (fun table ->
      Hashtbl.iter (Hashtbl.add table) l;
      Hashtbl.iter (Hashtbl.replace table) r)
end

type value =
(* Primitives *)
| Tvar          of typevar
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
and environment_ty = {
  parent_ty   : environment_ty option;
  bindings_ty : bool Table.t
}
and environment = {
  e_parent    : environment option;
  e_bindings  : (value * bool) Table.t;
}
and closure_ty = {
  args_ty     : value;
  kwargs_ty   : value;
  return_ty   : value;
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
  !lastvar

let rec typeof value =
  match value with
  | Int(_)
  -> IntTy

  | Symbol(_)
  -> SymbolTy

  | Tuple(xs)
  -> TupleTy(List.map typeof xs)

  | Record(xs)
  -> RecordTy(Table.map (fun v -> typeof v) xs)

  (* | Environment(e)
  -> EnvironmentTy(None, Table.map (fun (_, m) -> m) e.e_bindings)
 *)
  | Closure(c)
  -> ClosureTy(c.l_ty)

  | Instance(k,_)
  -> Class(k)
  | _
  -> assert false

let string_of_value value =
   (Sexplib.Sexp.to_string_hum (sexp_of_value value))

let inspect value =
  (string_of_value value) ^ " : " ^ (string_of_value (typeof value))

let create_env () =
  { e_parent   = None;
    e_bindings = Hashtbl.create 4 }

let exc_fail message loc =
  raise (Exc {
    ex_message    = message;
    ex_location   = loc;
    ex_highlights = [];
  })

let exc_type expected obj expr =
  exc_fail (expected ^ " expected; " ^ (inspect obj) ^ " found") (Syntax.loc expr)

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

let rec eval_tuple env elem =
  match elem with
  | Syntax.TupleElem(_,expr)
  -> Tuple [eval env expr]

  | Syntax.TupleSplice(_,expr)
  -> (match (eval env expr) with
      | Tuple(_) as t -> t
      | o -> exc_type "Tuple" o expr)

and eval_record env elem =
  match elem with
  | Syntax.RecordElem(_,k,v)
  -> Record (Table.pair k (eval env v))

  | Syntax.RecordSplice(_,expr)
  -> (match (eval env expr) with
     | Record(_) as r -> r
     | o -> exc_type "Record" o expr)

  | Syntax.RecordPair(_,k,v)
  -> (match (eval env k) with
     | Symbol(s) -> Record (Table.pair s (eval env v))
     | o -> exc_type "Symbol" o k)

and eval env expr =
  match expr with
  | Syntax.Int(_,x)
  -> Int(x)
  | Syntax.Sym(_,x)
  -> Symbol(x)
  | Syntax.Tuple(_,xs)
  -> List.fold_left concat_tuple (Tuple []) (List.map (eval_tuple env) xs)
  | Syntax.Record(_,xs)
  -> List.fold_left concat_record (Record (Table.empty ())) (List.map (eval_record env) xs)
  | _
  -> failwith ("cannot eval " ^ Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr));

