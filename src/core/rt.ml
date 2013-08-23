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
| Unsigned      of (*width*) int * big_int
| UnsignedTy    of (*width*) int
| Signed        of (*width*) int * big_int
| SignedTy      of (*width*) int
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
| ClosureTy     of ty list * ty
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
and type_env  = tvar Table.t
and const_env = package list
and lambda    = {
  l_hash          : int;
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
  im_hash         : int;
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
  kUnsigned         : klass;
  kSigned           : klass;
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

  let kPackage    = new_class "Package"
  in
  let roots = {
    last_tvar     = 0;

    kClass        = kClass;
    kMixin        = new_class "Mixin";
    kPackage      = kPackage;

    kTypeVariable = new_class "TypeVariable";
    kNil          = new_class "Nil";
    kBoolean      = new_class "Boolean";
    kInteger      = new_class "Integer";
    kSymbol       = new_class "Symbol";

    kUnsigned     = new_class "Unsigned";
    kSigned       = new_class "Signed";
    kTuple        = new_class "Tuple";
    kRecord       = new_class "Record";
    kLambda       = new_class "Lambda";

    pToplevel     = {
      p_name      = "toplevel";
      p_metaclass = empty_class "meta:toplevel" (Some kPackage) kClass;
      p_constants = Table.create []
    }
  }
  in
  let constants = roots.pToplevel.p_constants in
    List.iter (fun (k, v) ->
        Table.set constants k (Class (v, Table.create []))) [
      "Class",    roots.kClass;
      "Mixin",    roots.kMixin;
      "Package",  roots.kPackage;

      "TypeVariable", roots.kTypeVariable;
      "Nil",      roots.kNil;
      "Boolean",  roots.kBoolean;
      "Integer",  roots.kInteger;
      "Symbol",   roots.kSymbol;

      "Signed",   roots.kSigned;
      "Unsigned", roots.kUnsigned;
      "Tuple",    roots.kTuple;
      "Record",   roots.kRecord;
      "Lambda",   roots.kLambda;
    ];
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

let klass_of_type ?(dispatch=false) ty =
  match ty with
  | BooleanTy     -> !roots.kBoolean
  | NilTy         -> !roots.kNil

  | TvarTy        -> !roots.kTypeVariable
  | IntegerTy     -> !roots.kInteger
  | SymbolTy      -> !roots.kSymbol
  | UnsignedTy(_) -> !roots.kUnsigned
  | SignedTy(_)   -> !roots.kSigned
  | TupleTy(_)    -> !roots.kTuple
  | RecordTy(_)   -> !roots.kRecord

  | LambdaTy(_)   -> !roots.kLambda

  | Class(k,_)    -> k

  | _ -> failwith ("klass_of_type " ^
                   (Unicode.assert_utf8s
                    (Sexplib.Sexp.to_string_hum (sexp_of_value ty))))

let klass_of_value ?(dispatch=false) ?(meta=true) value =
  match value with
  | Truth | Lies  -> !roots.kBoolean
  | Nil           -> !roots.kNil

  | Tvar(_)       -> if meta then !roots.kTypeVariable else raise Not_found
  | Integer(_)    -> !roots.kInteger
  | Symbol(_)     -> !roots.kSymbol
  | Unsigned(_)   -> !roots.kUnsigned
  | Signed(_)     -> !roots.kSigned
  | Tuple(_)      -> !roots.kTuple
  | Record(_)     -> !roots.kRecord

  | Lambda(_)     -> !roots.kLambda

  | Instance((k,_),_) -> k
  | Class(k,_)    -> if dispatch then k.k_metaclass else !roots.kClass
  | Package(p)    -> if dispatch then p.p_metaclass else !roots.kPackage
  | Mixin(m,_)    -> if dispatch then m.m_metaclass else !roots.kMixin

  | BooleanTy | NilTy | TvarTy | IntegerTy | SymbolTy
  | TupleTy _ | RecordTy _ | LambdaTy _ | SignedTy _
  | UnsignedTy _
  -> klass_of_type ~dispatch value

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
  | Unsigned(w,_)   -> UnsignedTy(w)
  | Signed(w,_)     -> SignedTy(w)
  | Tuple(xs)       -> TupleTy (List.map type_of_value xs)
  | Record(xs)      -> RecordTy (Table.map (fun v -> type_of_value v) xs)

  | Environment(e)  -> EnvironmentTy (type_of_environment e)
  | Lambda(c)       -> LambdaTy c.l_ty

  | Package(p)      -> Class (p.p_metaclass, Table.create [])
  | Class(k,_)      -> Class (k.k_metaclass, Table.create [])
  | Instance(k,_)   -> Class k

  | BooleanTy | NilTy | TvarTy | IntegerTy | SymbolTy
  | TupleTy(_) | RecordTy(_) | LambdaTy(_)
  | SignedTy(_) | UnsignedTy(_)
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

let rec equal a b =
  let equal_list a b =
    List.fold_left2 (fun acc a b -> acc && equal a b) true a b
  in
  let equal_table a b =
    try  Table.fold2 ~f:(fun _ acc a b -> acc && equal a b) true a b
    with Invalid_argument _ -> (* different length *) false
  in
  let rec equal_local_env_ty a b =
    let equal_bindings_ty a b =
      try
        Table.fold2 ~f:(fun _ acc a b ->
            acc &&
              a.b_location_ty = b.b_location_ty &&
              a.b_kind_ty     = b.b_kind_ty &&
              equal a.b_value_ty b.b_value_ty)
          true a b
      with Invalid_argument _ -> (* different length *) false
    in
    let eq_parent =
      match a.e_parent_ty, b.e_parent_ty with
      | None, None -> true
      | Some a, Some b -> equal_local_env_ty a b
      | _, _ -> false
    in
    eq_parent && equal_bindings_ty a.e_bindings_ty b.e_bindings_ty
  in
  match a, b with
  (* Immutable values and types. *)
  | TvarTy,             TvarTy
  | Nil,                Nil
  | NilTy,              NilTy
  | Truth,              Truth
  | Lies,               Lies
  | BooleanTy,          BooleanTy
  | IntegerTy,          IntegerTy
  | SymbolTy,           SymbolTy
  | BasicBlockTy,       BasicBlockTy
  -> true

  (* Immutable values and types with immutable parameters. *)
  | Tvar(a),            Tvar(b)
  -> a = b
  | Integer(a),         Integer(b)
  -> a = b
  | Symbol(a),          Symbol(b)
  -> a = b
  | UnsignedTy(a),      UnsignedTy(b)
  | SignedTy(a),        SignedTy(b)
  -> a = b
  | Unsigned(wa,a),     Unsigned(wb,b)
  | Signed(wa,a),       Signed(wb,b)
  -> wa = wb && a = b

  (* Immutable values and types with possibly mutable parameters. *)
  | Tuple(a),           Tuple(b)
  | TupleTy(a),         TupleTy(b)
  -> equal_list a b
  | Record(a),          Record(b)
  | RecordTy(a),        RecordTy(b)
  -> equal_table a b
  | FunctionTy(aa,ar),  FunctionTy(ba,br)
  | ClosureTy(aa,ar),   ClosureTy(ba,br)
  -> equal_list aa ba && equal ar br
  | LambdaTy(a),        LambdaTy(b)
  -> (equal a.l_args_ty b.l_args_ty &&
        equal a.l_kwargs_ty b.l_kwargs_ty &&
        equal a.l_result_ty b.l_result_ty)

  (* Mutable values and types. *)
  | EnvironmentTy(a),   EnvironmentTy(b)
  -> equal_local_env_ty a b
  | Environment(a),     Environment(b)
  -> a == b
  | Lambda(a),          Lambda(b)
  -> a == b
  | Package(a),         Package(b)
  -> a == b
  | Instance(_,a),      Instance(_,b)
  -> a == b
  | Class(a,sa),        Class(b,sb)
  -> a == b && equal_table sa sb
  | Mixin(a,sa),        Mixin(b,sb)
  -> a == b && equal_table sa sb
  | _, _
  -> false

(* Inspecting types and values *)

let string_of_value value =
  (Unicode.assert_utf8s
    (Sexplib.Sexp.to_string_hum (sexp_of_value value)))

let inspect_literal_or value f =
  match value with
  | TvarTy        -> "TypeVariable"
  | Truth         -> "true"
  | Lies          -> "false"
  | BooleanTy     -> "Boolean"
  | Nil           -> "nil"
  | NilTy         -> "Nil"
  | Integer(n)    -> (string_of_big_int n) ^ "i"
  | IntegerTy     -> "Integer"
  | Symbol(s)     -> ":" ^ s
  | SymbolTy      -> "Symbol"
  | Unsigned(w,v) -> (string_of_big_int v) ^ "u" ^ (string_of_int w)
  | UnsignedTy(w) -> "Unsigned(" ^ (string_of_int w) ^ ")"
  | Signed(w,v)   -> (string_of_big_int v) ^ "s" ^ (string_of_int w)
  | SignedTy(w)   -> "Signed(" ^ (string_of_int w) ^ ")"
  | Class(k,_)    -> k.k_name ^ "()"
  | _             -> f value

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
    | TupleTy(_) | RecordTy(_) | LambdaTy(_)
    -> "type " ^ (inspect_type value)
    | Package(p) -> p.p_name
    | _ -> (string_of_value value))

and inspect_type ty =
  inspect_literal_or ty (fun x ->
    match ty with
    | Tvar(tv)     -> "\\" ^ (string_of_int tv)
    | TupleTy(xs)
    -> "[" ^ (String.concat ", " (List.map inspect_type xs)) ^ "]"
    | RecordTy(xs)
    -> "{" ^ (String.concat ", " (Table.map_list
                (fun k v -> k ^ ": " ^ (inspect_type v)) xs)) ^ "}"
    | LambdaTy(lm)
    -> (let args_ty =
          match lm.l_args_ty with
          | TupleTy(xs) -> List.map inspect_type xs
          | o -> ["*" ^ (inspect_type o)]
        in let kwargs_ty =
          match lm.l_kwargs_ty with
          | RecordTy(xs) -> Table.map_list
                              (fun k v -> k ^ ": " ^ (inspect_type v)) xs
          | o -> ["**" ^ (inspect_type o)]
        in "(" ^ (String.concat ", " (args_ty @ kwargs_ty)) ^
           ") -> " ^ (inspect_type lm.l_result_ty))
    | _            -> "\\(" ^ (inspect_value ty) ^ ")")

let inspect value =
  let ty =
    try  (inspect_type (type_of_value value))
    with Failure(_) -> "#<untypable value>"
  in (inspect_value value) ^ " : " ^ ty

let print_type value =
  print_endline (inspect_type value)

let print_value value =
  print_endline (inspect_value value)

let print value =
  print_endline (inspect value)

(* Exceptions *)

let exc_fail message locations =
  raise (Exc {
    ex_message    = message;
    ex_locations  = locations;
  })

let exc_type expected obj =
  exc_fail (expected ^ " expected; " ^ (inspect obj) ^ " found")
