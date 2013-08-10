open Sexplib.Std
open Unicode.Std
open Big_int

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
| Integer       of big_int
| IntegerTy
| Symbol        of string
| SymbolTy
(* Product types *)
| Tuple         of value list
| TupleTy       of ty list
| Record        of value Table.t
| RecordTy      of ty Table.t
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
| Instance      of klass specialized * slots
(* SSA types *)
| FunctionTy    of ty list * ty
| BasicBlockTy
and ty = value
and 'a specialized = 'a * value Table.t
and slots = value Table.t
and binding_ty = {
  b_location_ty   : Location.t;
  b_kind_ty       : Syntax.lvar_kind;
  b_value_ty      : ty;
}
and bindings_ty  = binding_ty Table.t
and local_env_ty = {
  e_parent_ty     : local_env_ty option;
  e_bindings_ty   : bindings_ty;
}
and binding = {
  b_location      : Location.t;
  b_kind          : Syntax.lvar_kind;
  b_value         : value;
}
and bindings  = binding Table.t
and local_env = {
  e_parent        : local_env option;
  e_bindings      : bindings;
}
and type_env  =     tvar Table.t
and const_env =     package list
and lambda    = {
  l_location      : Location.t;
  l_ty            : lambda_ty;
  mutable l_local_env : local_env;
  mutable l_type_env  : type_env;
  mutable l_const_env : const_env;
  l_args          : Syntax.formal_args;
  l_body          : Syntax.exprs;
}
and lambda_ty = {
  l_args_ty       : ty;
  l_kwargs_ty     : ty;
  l_result_ty     : ty;
}
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
  iv_ty           : ty;
}
and exc = {
  ex_message      : string;
  ex_locations    : Location.t list;
}
with sexp_of

exception Exc of exc
with sexp

(* Class tooling & default virtual image *)

type roots = {
  mutable last_tvar : int;

  kClass            : klass;
  kTypeVariable     : klass;
  kNil              : klass;
  kBoolean          : klass;
  kInteger          : klass;
  kSymbol           : klass;
  kTuple            : klass;
  kRecord           : klass;
  kLambda           : klass;
  kMixin            : klass;
  kPackage          : klass;

  pToplevel         : package;
}

let empty_class name ancestor metaclass =
  { k_name      = name;
    k_ancestor  = ancestor;
    k_metaclass = metaclass;
    k_tvars     = Table.create [];
    k_ivars     = Table.create [];
    k_methods   = Table.create [];
    k_prepended = [];
    k_appended  = []; }

let create_class () =
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
  in
  (kClass, kmetaClass)

let create_roots () =
  let (kClass, kmetaClass) = create_class ()
  in
  let new_class ?ancestor name =
    let meta_ancestor =
      Option.map_default (fun k -> k.k_ancestor) None ancestor
    in empty_class name ancestor
        (empty_class ("meta:" ^ name) meta_ancestor kClass)
  in

  let kMixin      = new_class "Mixin"   in
  let kPackage    = new_class "Package" in

  let kTuple      = new_class "Tuple"  in
  let kRecord     = new_class "Record" in
  let kLambda     = new_class "Lambda" in

  let roots = {
    last_tvar     = 0;

    kClass        = kClass;
    kMixin        = kMixin;
    kPackage      = kPackage;

    kTypeVariable = new_class "TypeVariable";
    kNil          = new_class "Nil";
    kBoolean      = new_class "Boolean";
    kInteger      = new_class "Integer";
    kSymbol       = new_class "Symbol";
    kTuple        = kTuple;
    kRecord       = kRecord;
    kLambda       = kLambda;

    pToplevel     = {
      p_name      = "toplevel";
      p_metaclass = empty_class "meta:toplevel" (Some kPackage) kClass;
      p_constants = Table.create [
        ("Class",        Class (kClass, Table.create []));
        ("Mixin",        Class (kMixin, Table.create []));
        ("Package",      Class (kPackage, Table.create []));

        ("TypeVariable", TvarTy);
        ("Nil",          NilTy);
        ("Boolean",      BooleanTy);
        ("Integer",      IntegerTy);
        ("Symbol",       SymbolTy);

        ("Tuple",        Class (kTuple, Table.create []));
        ("Record",       Class (kRecord, Table.create []));
        ("Lambda",       Class (kLambda, Table.create []));
      ]
    }
  }
  in

  roots

let roots = ref (create_roots ())

let adopt_tvar value : tvar =
  value

let new_tvar () : tvar =
  let roots = !roots in
  roots.last_tvar <- roots.last_tvar + 1;
  roots.last_tvar

let new_class ?ancestor name =
  let meta_ancestor =
    Option.map_default (fun k -> k.k_ancestor) None ancestor
  in empty_class name ancestor
      (empty_class ("meta:" ^ name) meta_ancestor !roots.kClass)

let new_package name =
  {
    p_name      = name;
    p_metaclass = empty_class ("meta:" ^ name) (Some !roots.kPackage) !roots.kClass;
    p_constants = Table.create [];
  }

(* Types and classes *)

let klass_of_type ty =
  match ty with
  | BooleanTy     -> !roots.kBoolean
  | NilTy         -> !roots.kNil

  | TvarTy        -> !roots.kTypeVariable
  | IntegerTy     -> !roots.kInteger
  | SymbolTy      -> !roots.kSymbol
  | TupleTy(_)    -> !roots.kTuple
  | RecordTy(_)   -> !roots.kRecord

  | LambdaTy(_)   -> !roots.kLambda

  | Class(k,_)    -> k
  | _ -> failwith ("klass_of_type " ^
                   (Unicode.assert_utf8s
                    (Sexplib.Sexp.to_string_hum (sexp_of_value ty))))

let klass_of_value ?(dispatch=false) value =
  match value with
  | Truth | Lies  -> !roots.kBoolean
  | Nil           -> !roots.kNil

  | Tvar(_)       -> !roots.kTypeVariable
  | Integer(_)    -> !roots.kInteger
  | Symbol(_)     -> !roots.kSymbol
  | Tuple(_)      -> !roots.kTuple
  | Record(_)     -> !roots.kRecord

  | Lambda(_)     -> !roots.kLambda

  | Instance((k,_),_) -> k
  | Class(k,_)    -> if dispatch then k.k_metaclass else !roots.kClass
  | Package(p)    -> if dispatch then p.p_metaclass else !roots.kPackage
  | Mixin(m,_)    -> if dispatch then m.m_metaclass else !roots.kMixin

  | BooleanTy | NilTy | TvarTy | IntegerTy | SymbolTy
  | TupleTy(_) | RecordTy(_) | LambdaTy(_)
  -> (let klass = klass_of_type value in
        if dispatch then
          klass.k_metaclass
        else
          klass)

  | _ -> failwith ("klass_of_value " ^
                   (Unicode.assert_utf8s
                    (Sexplib.Sexp.to_string_hum (sexp_of_value value))))

let rec type_of_value value =
  match value with
  | Truth | Lies    -> BooleanTy
  | Nil             -> NilTy

  | Tvar(_)         -> TvarTy
  | Integer(_)      -> IntegerTy
  | Symbol(_)       -> SymbolTy
  | Tuple(xs)       -> TupleTy (List.map type_of_value xs)
  | Record(xs)      -> RecordTy (Table.map (fun v -> type_of_value v) xs)

  | Environment(e)  -> EnvironmentTy (type_of_environment e)
  | Lambda(c)       -> LambdaTy c.l_ty

  | Package(_)      -> Class (!roots.kPackage, Table.create [])
  | Class(k,_)      -> Class (!roots.kClass, Table.create [])
  | Instance(k,_)   -> Class (k)

  | BooleanTy | NilTy | TvarTy | IntegerTy | SymbolTy
  | TupleTy(_) | RecordTy(_) | LambdaTy(_)
  -> Class (klass_of_type value, Table.create [])

  | _ -> failwith ("type_of_value " ^
                   (Unicode.assert_utf8s
                    (Sexplib.Sexp.to_string_hum (sexp_of_value value))))

and type_of_environment env =
  {
    e_parent_ty   = Option.map type_of_environment env.e_parent;
    e_bindings_ty =
      Table.map (fun b -> {
        b_location_ty = b.b_location;
        b_kind_ty     = b.b_kind;
        b_value_ty    = type_of_value b.b_value;
      }) env.e_bindings;
  }

let specialize conv ty =
  assert false

(* Inspecting types and values *)

let string_of_value value =
   (Unicode.assert_utf8s
    (Sexplib.Sexp.to_string_hum (sexp_of_value value)))

let inspect_literal_or value f =
  match value with
  | Truth      -> "true"
  | Lies       -> "false"
  | Nil        -> "nil"
  | Integer(n) -> string_of_big_int n
  | Symbol(s)  -> ":" ^ s
  | _          -> f value

let rec inspect_value value =
  inspect_literal_or value (fun x ->
    match value with
    | Tuple(xs)
    -> "[" ^ (String.concat ", " (List.map inspect_value xs)) ^ "]"
    | Record(xs)
    -> "{" ^ (String.concat ", " (Table.map_list
                (fun k v -> k ^ ": " ^ (inspect_value v)) xs)) ^ "}"
    | Lambda(lm)
    -> "#<Lambda " ^ Location.at(lm.l_location) ^ ">"
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
           ") -> " ^ (inspect_type lm.l_result_ty))
    | Class(k,_)   -> k.k_name
    | _            -> "((" ^ (inspect_value ty) ^ "))")

let inspect value =
  let ty =
    try  (inspect_type (type_of_value value))
    with Failure(_) -> "#<untypable value>"
  in (inspect_value value) ^ " : " ^ ty

(* Exceptions *)

let exc_fail message locations =
  raise (Exc {
    ex_message    = message;
    ex_locations  = locations;
  })

let exc_type expected obj =
  exc_fail (expected ^ " expected; " ^ (inspect obj) ^ " found")
