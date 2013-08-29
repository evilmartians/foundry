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
| String        of string
| StringTy
(* Product types *)
| Tuple         of value list
| TupleTy       of ty    list
| Record        of value Assoc.sorted_t
| RecordTy      of ty    Assoc.sorted_t
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
and 'a specialized = 'a * value Assoc.sorted_t
and slots          = value Table.t
and binding_ty = {
          b_ty_location   : Location.t;
          b_ty_kind       : Syntax.lvar_kind;
          b_ty            : ty;
}
and bindings_ty  = binding_ty Table.t
and local_env_ty = {
          e_ty_parent     : local_env_ty option;
          e_ty_bindings   : bindings_ty;
}
and binding = {
          b_location      : Location.t;
          b_kind          : Syntax.lvar_kind;
          b_value         : value;
}
and bindings  = binding Table.t
and local_env = {
          e_hash          : int;
          e_parent        : local_env option;
          e_bindings      : bindings;
}
and type_env  = tvar Table.t
and const_env = package list
and lambda    = {
          l_hash          : int;
          l_location      : Location.t;
  mutable l_ty            : lambda_ty;
  mutable l_local_env     : local_env;
  mutable l_type_env      : type_env;
  mutable l_const_env     : const_env;
          l_args          : Syntax.formal_args;
          l_body          : Syntax.exprs;
}
and lambda_ty = {
          l_ty_args       : ty;
          l_ty_kwargs     : ty;
          l_ty_result     : ty;
}
and package = {
          p_hash          : int;
          p_name          : string;
          p_metaclass     : klass;
          p_constants     : value Table.t;
}
and klass = {
          k_hash          : int;
          k_name          : string;
          k_metaclass     : klass;
  mutable k_objectclass   : klass option;
          k_ancestor      : klass option;
          k_is_value      : bool;
          k_parameters    : tvar    Assoc.sequental_t;
  mutable k_ivars         : ivar    Assoc.sequental_t;
  mutable k_methods       : imethod Assoc.sequental_t;
  mutable k_prepended     : mixin list;
  mutable k_appended      : mixin list;
}
and mixin = {
          m_hash          : int;
          m_name          : string;
          m_metaclass     : klass;
  mutable m_methods       : imethod Assoc.sequental_t;
}
and imethod = {
          im_hash         : int;
          im_body         : lambda;
          im_dynamic      : bool;
}
and ivar = {
          iv_hash         : int;
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
  kObject           : klass;
  kValue            : klass;
  kTypeVariable     : klass;
  kNil              : klass;
  kBoolean          : klass;
  kInteger          : klass;
  kSymbol           : klass;
  kString           : klass;
  kUnsigned         : klass;
  kSigned           : klass;
  kTuple            : klass;
  kRecord           : klass;
  kLambda           : klass;
  kMixin            : klass;
  kPackage          : klass;

  pToplevel         : package;
}

(* Empty regular class -- e.g. for "class Foo". *)

let empty_class kClass ?ancestor ?(parameters=Assoc.empty) name =
  let rec klass =
    { k_hash        = Hash_seed.make ();
      k_name        = name;
      k_ancestor    = ancestor;
      k_metaclass   = metaklass;
      k_objectclass = None;
      k_is_value    = Option.map_default (fun k -> k.k_is_value) false ancestor;
      k_parameters  = parameters;
      k_ivars       = Assoc.empty;
      k_methods     = Assoc.empty;
      k_prepended   = [];
      k_appended    = []; }
  and metaklass =
    { k_hash        = Hash_seed.make ();
      k_name        = "meta:" ^ name;
      k_ancestor    = Some (Option.map_default (fun k -> k.k_metaclass) kClass ancestor);
      k_metaclass   = kClass;
      k_objectclass = Some klass;
      k_is_value    = false;
      k_parameters  = parameters;
      k_ivars       = Assoc.empty;
      k_methods     = Assoc.empty;
      k_prepended   = [];
      k_appended    = []; }
  in
  klass

(* Empty metaclass for a non-class object -- e.g. "package Foo". *)

let empty_metaclass kClass ~ancestor name =
  { k_hash        = Hash_seed.make ();
    k_name        = "meta:" ^ name;
    k_ancestor    = Some ancestor;
    k_metaclass   = kClass;
    k_objectclass = None;
    k_is_value    = false;
    k_parameters  = Assoc.empty;
    k_ivars       = Assoc.empty;
    k_methods     = Assoc.empty;
    k_prepended   = [];
    k_appended    = []; }

(* Empty class Class for bootstrapping. *)

let create_class () =
  let rec kClass =
    { k_hash        = Hash_seed.make ();
      k_name        = "Class";
      k_ancestor    = None;
      k_metaclass   = kmetaClass;
      k_objectclass = None;
      k_is_value    = false;
      k_parameters  = Assoc.empty;
      k_ivars       = Assoc.empty;
      k_methods     = Assoc.empty;
      k_prepended   = [];
      k_appended    = []; }
  and kmetaClass =
    { k_hash        = Hash_seed.make ();
      k_name        = "meta:Class";
      k_ancestor    = Some kClass;
      k_metaclass   = kClass;
      k_objectclass = Some kClass;
      k_is_value    = false;
      k_parameters  = Assoc.empty;
      k_ivars       = Assoc.empty;
      k_methods     = Assoc.empty;
      k_prepended   = [];
      k_appended    = []; }
  in
  (kClass, kmetaClass)

let create_roots () =
  let (kClass, kmetaClass) = create_class () in
  let last_tvar = ref 0 in
  let tvar () =
    last_tvar := !last_tvar + 1;
    (!last_tvar : tvar)
  in
  let new_class ?ancestor ?parameters name =
    empty_class kClass ?ancestor ?parameters name
  in

  let kObject     = new_class "Object" in
  let kValue      = new_class "Value"  in
  let kValue      = { kValue with k_is_value = true } in

  let kPackage    = new_class ~ancestor:kObject "Package" in

  let roots = {
    last_tvar     = !last_tvar;

    kClass        = kClass;
    kObject       = kObject;
    kValue        = kValue;

    kTypeVariable = new_class ~ancestor:kValue "TypeVariable";
    kNil          = new_class ~ancestor:kValue "Nil";
    kBoolean      = new_class ~ancestor:kValue "Boolean";
    kInteger      = new_class ~ancestor:kValue "Integer";
    kSymbol       = new_class ~ancestor:kValue "Symbol";
    kString       = new_class ~ancestor:kObject "String";

    kUnsigned     = new_class ~ancestor:kValue
                              ~parameters:(Assoc.sequental ["width", tvar ()])
                              "Unsigned";

    kSigned       = new_class ~ancestor:kValue
                              ~parameters:(Assoc.sequental ["width", tvar ()])
                              "Signed";

    kTuple        = new_class ~ancestor:kValue "Tuple";
    kRecord       = new_class ~ancestor:kValue "Record";
    kLambda       = new_class ~ancestor:kValue "Lambda";

    kMixin        = new_class ~ancestor:kObject "Mixin";
    kPackage      = kPackage;

    pToplevel     = {
      p_hash      = Hash_seed.make ();
      p_name      = "toplevel";
      p_metaclass = empty_metaclass kClass ~ancestor:kPackage "toplevel";
      p_constants = Table.create []
    }
  }
  in
  let constants = roots.pToplevel.p_constants in
  List.iter (fun (k, v) ->
      Table.set constants k (Class (v, Assoc.empty)))
    [
      "Class",        roots.kClass;
      "Object",       roots.kObject;
      "Value",        roots.kValue;

      "TypeVariable", roots.kTypeVariable;
      "Nil",          roots.kNil;
      "Boolean",      roots.kBoolean;
      "Integer",      roots.kInteger;
      "Symbol",       roots.kSymbol;
      "String",       roots.kString;

      "Signed",       roots.kSigned;
      "Unsigned",     roots.kUnsigned;
      "Tuple",        roots.kTuple;
      "Record",       roots.kRecord;
      "Lambda",       roots.kLambda;

      "Mixin",        roots.kMixin;
      "Package",      roots.kPackage;
    ];
  roots

let roots = ref (create_roots ())

let adopt_tvar value : tvar =
  value

let new_tvar () : tvar =
  let roots = !roots in
  roots.last_tvar <- roots.last_tvar + 1;
  roots.last_tvar

let last_static_tvar = ref 0

let new_static_tvar () : tvar =
  last_static_tvar := !last_static_tvar - 1;
  !last_static_tvar

let new_class = empty_class !roots.kClass

let new_package name =
  { p_hash      = Hash_seed.make ();
    p_name      = name;
    p_metaclass = empty_metaclass !roots.kClass ~ancestor:!roots.kPackage name;
    p_constants = Table.create []; }

(* Types and classes *)

let klass_of_type ?(dispatch=false) ty =
  match ty with
  | BooleanTy     -> !roots.kBoolean
  | NilTy         -> !roots.kNil

  | TvarTy        -> !roots.kTypeVariable
  | IntegerTy     -> !roots.kInteger
  | SymbolTy      -> !roots.kSymbol
  | StringTy      -> !roots.kString
  | UnsignedTy(_) -> !roots.kUnsigned
  | SignedTy(_)   -> !roots.kSigned
  | TupleTy(_)    -> !roots.kTuple
  | RecordTy(_)   -> !roots.kRecord

  | LambdaTy(_)   -> !roots.kLambda
  | ClosureTy(_)  -> !roots.kLambda

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
  | String(_)     -> !roots.kString
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
  | String(_)       -> StringTy
  | Unsigned(w,_)   -> UnsignedTy(w)
  | Signed(w,_)     -> SignedTy(w)
  | Tuple(xs)       -> TupleTy (List.map type_of_value xs)
  | Record(xs)      -> RecordTy (Assoc.map (fun _ -> type_of_value) xs)

  | Environment(e)  -> EnvironmentTy (type_of_environment e)
  | Lambda(c)       -> LambdaTy c.l_ty

  | Package(p)      -> Class (p.p_metaclass, Assoc.empty)
  | Class(k,sp)     -> Class (k.k_metaclass, sp)
  | Instance(k,_)   -> Class k

  | BooleanTy | NilTy | TvarTy | IntegerTy | SymbolTy
  | TupleTy(_) | RecordTy(_) | LambdaTy(_)
  | SignedTy(_) | UnsignedTy(_)
  -> Class (klass_of_type value, Assoc.empty)

  | _ -> failwith ("type_of_value " ^
                   (Unicode.assert_utf8s
                    (Sexplib.Sexp.to_string_hum (sexp_of_value value))))

and type_of_environment ?(imm=true) env =
  let bindings =
    if not imm then
      Table.filter env.e_bindings ~f:(fun _ b ->
        b.b_kind = Syntax.LVarMutable)
    else
      env.e_bindings
  in
  { e_ty_parent   = Option.map (type_of_environment ~imm) env.e_parent;
    e_ty_bindings =
      Table.map (fun b -> {
        b_ty_location = b.b_location;
        b_ty_kind     = b.b_kind;
        b_ty          = type_of_value b.b_value;
      }) bindings; }

let rec equal_local_env_ty a b =
  let equal_bindings_ty a b =
    try
      Table.fold2 ~f:(fun _ acc a b ->
          acc &&
            a.b_ty_location = b.b_ty_location &&
            a.b_ty_kind     = b.b_ty_kind &&
            equal a.b_ty b.b_ty)
        true a b
    with Invalid_argument _ -> (* different length *) false
  in
  let eq_parent =
    match a.e_ty_parent, b.e_ty_parent with
    | None, None -> true
    | Some a, Some b -> equal_local_env_ty a b
    | _, _ -> false
  in
  eq_parent && equal_bindings_ty a.e_ty_bindings b.e_ty_bindings

and equal a b =
  let equal_list a b =
    try  List.fold_left2 (fun acc a b -> acc && equal a b) true a b
    with Invalid_argument _ -> (* different length *) false
  in
  match a, b with
  (* Shortcut for physical equality (fast check). *)
  | _, _ when a == b
  -> true

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
  | String(a),          String(b)
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
  -> Assoc.equal ~eq:equal a b
  | FunctionTy(aa,ar),  FunctionTy(ba,br)
  | ClosureTy(aa,ar),   ClosureTy(ba,br)
  -> equal_list aa ba && equal ar br
  | LambdaTy(a),        LambdaTy(b)
  -> (equal a.l_ty_args b.l_ty_args &&
        equal a.l_ty_kwargs b.l_ty_kwargs &&
        equal a.l_ty_result b.l_ty_result)

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
  | Class(a, sa),       Class(b, sb)
  -> a == b && Assoc.equal ~eq:equal sa sb
  | Mixin(a, sa),       Mixin(b, sb)
  -> a == b && Assoc.equal ~eq:equal sa sb
  | _, _
  -> false

let (=^-^=) = equal

let rec hash_local_env_ty env =
  let seed =
    match env.e_ty_parent with
    | Some env -> hash_local_env_ty env
    | None     -> 0
  in
  Hashtbl.hash (seed, (Table.map_list ~ordered:true
                        ~f:(fun k _ -> k) env.e_ty_bindings))

let rec hash value =
  let hash_list lst =
    Hashtbl.hash (List.map hash lst)
  in
  let hash_assoc (assoc : 'a Assoc.sorted_t) =
    Hashtbl.hash (Assoc.keys assoc)
  in
  match value with
  (* Immutable values and types. *)
  | TvarTy | Nil | NilTy | Truth | Lies
  | BooleanTy | IntegerTy | SymbolTy | StringTy
  | BasicBlockTy
  (* Immutable values and types with immutable parameters. *)
  | Tvar _ | Integer _ | Symbol _ | String _
  | UnsignedTy _ | SignedTy _ | Unsigned _ | Signed _
  -> Hashtbl.hash value

  (* Immutable values and types with possibly mutable parameters. *)
  | Tuple(x) | TupleTy(x)
  -> hash_list x
  | Record(x) | RecordTy(x)
  -> hash_assoc x
  | EnvironmentTy(x)
  -> hash_local_env_ty x
  | LambdaTy(x)
  -> hash_list [x.l_ty_args; x.l_ty_kwargs; x.l_ty_result]
  | FunctionTy(xa, xr) | ClosureTy(xa, xr)
  -> hash_list (xr :: xa)

  (* Mutable values and types. *)
  | Environment(x)
  -> x.e_hash
  | Lambda(x)
  -> x.l_hash
  | Package(x)
  -> x.p_hash
  | Instance((k, sp), _)
  -> Hashtbl.hash [k.k_hash; hash_assoc sp]
  | Class(x, sp)
  -> Hashtbl.hash [x.k_hash; hash_assoc sp]
  | Mixin(x, sp)
  -> Hashtbl.hash [x.m_hash; hash_assoc sp]

module ValueIdentity =
struct
  type t = value

  let equal = equal
  let hash  = hash
end

module Valuetbl = Hashtbl.Make(ValueIdentity)

module EnvTyIdentity =
struct
  type t = local_env_ty

  let equal = equal_local_env_ty
  let hash  = hash_local_env_ty
end

module EnvTytbl = Hashtbl.Make(EnvTyIdentity)

(* Inspecting types and values *)

let string_of_value value =
  (Unicode.assert_utf8s
    (Sexplib.Sexp.to_string_hum (sexp_of_value value)))

let rec inspect_literal_or value f =
  match value with
  | TvarTy        -> "TypeVariable"
  | Truth         -> "true"
  | Lies          -> "false"
  | BooleanTy     -> "Boolean"
  | Nil           -> "nil"
  | NilTy         -> "Nil"
  | Integer(n)    -> (string_of_big_int n)
  | IntegerTy     -> "Integer"
  | Symbol(s)     -> ":" ^ s
  | SymbolTy      -> "Symbol"
  | String(s)     -> "\"" ^ s ^ "\""
  | StringTy      -> "String"
  | Unsigned(w,v) -> (string_of_big_int v) ^ "u" ^ (string_of_int w)
  | UnsignedTy(w) -> "Unsigned(" ^ (string_of_int w) ^ ")"
  | Signed(w,v)   -> (string_of_big_int v) ^ "s" ^ (string_of_int w)
  | SignedTy(w)   -> "Signed(" ^ (string_of_int w) ^ ")"
  | Class(k,sp)   -> k.k_name ^ "(" ^ (String.concat ", " (Assoc.map_list
                          (fun k v -> k ^ ": " ^ (inspect_type v)) sp)) ^ ")"
  | Instance(c,s) -> "#<" ^ (inspect_type (Class c)) ^ (String.concat "" (Table.map_list
                          (fun k v -> " @" ^ k ^ "=" ^ (inspect_value v)) s) ^ ">")
  | _             -> f value

and inspect_value value =
  inspect_literal_or value (fun x ->
    match value with
    | Tuple(xs)
    -> "[" ^ (String.concat ", " (List.map inspect_value xs)) ^ "]"
    | Record(xs)
    -> "{" ^ (String.concat ", " (Assoc.map_list
                (fun k v -> k ^ ": " ^ (inspect_value v)) xs)) ^ "}"
    | Lambda(lm)
    -> "#<Lambda " ^ Location.at(lm.l_location) ^ ">"
    | TupleTy(_) | RecordTy(_) | LambdaTy(_)
    -> "type " ^ (inspect_type value)
    | Package(p) -> p.p_name
    | _ -> string_of_value value)

and inspect_type ty =
  inspect_literal_or ty (fun x ->
    match ty with
    | Tvar(tv)     -> "\\" ^ (string_of_int tv)
    | TupleTy(xs)
    -> "[" ^ (String.concat ", " (List.map inspect_type xs)) ^ "]"
    | RecordTy(xs)
    -> "{" ^ (String.concat ", " (Assoc.map_list
                (fun k v -> k ^ ": " ^ (inspect_type v)) xs)) ^ "}"
    | LambdaTy(lm)
    -> (let args_ty =
          match lm.l_ty_args with
          | TupleTy(xs) -> List.map inspect_type xs
          | o -> ["*" ^ (inspect_type o)]
        in let kwargs_ty =
          match lm.l_ty_kwargs with
          | RecordTy(xs) -> Assoc.map_list
                              (fun k v -> k ^ ": " ^ (inspect_type v)) xs
          | o -> ["**" ^ (inspect_type o)]
        in "(" ^ (String.concat ", " (args_ty @ kwargs_ty)) ^
           ") -> " ^ (inspect_type lm.l_ty_result))
    | EnvironmentTy(env)
    -> "`env " ^ (inspect_local_env_ty env)
    | FunctionTy(args_ty, result_ty)
    -> "`fun (" ^ (String.concat ", " (List.map inspect_type args_ty)) ^
                ") -> " ^ (inspect_type result_ty)
    | ClosureTy(args_ty, result_ty)
    -> "`lam (" ^ (String.concat ", " (List.map inspect_type args_ty)) ^
                ") -> " ^ (inspect_type result_ty)
    | _
    -> "\\(" ^ (inspect_value ty) ^ ")")

and inspect_local_env_ty env =
  "{" ^
    (String.concat ", "
      (Table.map_list (fun k v ->
          k ^ ": " ^ (inspect_type v.b_ty))
        env.e_ty_bindings)) ^
  "}" ^
    match env.e_ty_parent with
    | Some parent -> " -> " ^ (inspect_local_env_ty parent)
    | None -> ""

let inspect value =
  let ty =
    try  (inspect_type (type_of_value value))
    with Failure(_) -> "#<untypable value>"
  in (inspect_value value) ^ " : " ^ ty

let print_type value =
  prerr_endline (inspect_type value)

let print_value value =
  prerr_endline (inspect_value value)

let print value =
  prerr_endline (inspect value)

(* Exceptions *)

let exc_fail message locations =
  raise (Exc {
    ex_message    = message;
    ex_locations  = locations;
  })

let exc_type expected obj =
  exc_fail (expected ^ " expected; " ^ (inspect obj) ^ " found")

(* Local environment *)

let lenv_create parent : local_env =
  { e_hash     = Hash_seed.make ();
    e_parent   = parent;
    e_bindings = Table.create [] }

let lenv_bind env name ~kind ~value ~loc =
  match Table.get env.e_bindings name with
  | Some(b)
  -> assert false
  | None
  -> Table.set env.e_bindings name {
    b_location = loc;
    b_kind     = kind;
    b_value    = value;
  }

let rec lenv_mutate env name ~value =
  match Table.get env.e_bindings name with
  | Some({ b_kind = Syntax.LVarImmutable })
  -> assert false
  | Some b
  -> Table.set env.e_bindings name {
    b_location = b.b_location;
    b_kind     = Syntax.LVarMutable;
    b_value    = value;
  }
  | None
  -> match env.e_parent with
     | Some parent -> lenv_mutate parent name value
     | None -> assert false

let rec lenv_lookup env name =
  match Table.get env.e_bindings name with
  | Some({ b_value = value }) -> value
  | None
  -> match env.e_parent with
     | Some parent -> lenv_lookup parent name
     | None -> assert false

(* Type environment *)

let tenv_create () : type_env =
  Table.create []

let tenv_fork env =
  Table.copy env

let tenv_resolve env name =
  match Table.get env name with
  | Some tvar -> tvar
  | None ->
    let tvar = new_tvar () in
      Table.set env name tvar;
      tvar

(* Constant environment *)

exception CEnvUnbound
exception CEnvAlreadyBound of value

let cenv_create () : const_env =
  [!roots.pToplevel]

let cenv_extend env pkg =
  pkg :: env

let cenv_bind env name value =
  let pkg = List.hd env in
    match Table.get pkg.p_constants name with
    | Some value -> raise (CEnvAlreadyBound value)
    | None -> Table.set pkg.p_constants name value

let cenv_peek env name =
  let pkg = List.hd env in
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

  in lookup env
