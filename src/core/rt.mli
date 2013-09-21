open Unicode.Std
open Big_int

type tvar = private int
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
(* Complex types *)
| Option        of value option
| OptionTy      of ty
| Tuple         of value list
| TupleTy       of ty    list
| Record        of value Assoc.sorted_t
| RecordTy      of ty    Assoc.sorted_t
| Array         of ty * value DynArray.t
| ArrayTy       of ty
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
| Instance      of instance
(* SSA types *)
| FunctionTy    of ty list * ty
| BasicBlockTy
and ty = value
and 'a specialized = 'a * ty Assoc.sorted_t
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
  (* Mutable for deserialization. *)
  mutable l_ty            : lambda_ty;
  mutable l_local_env     : local_env;
  mutable l_type_env      : type_env;
  mutable l_const_env     : const_env;
          l_args          : lambda_args;
          l_body          : Syntax.exprs;
}
and lambda_ty_elem =
| LambdaArg       of ty
| LambdaOptArg    of ty
| LambdaRest      of ty
| LambdaKwArg     of string * ty
| LambdaKwOptArg  of string * ty
| LambdaKwRest    of ty
and lambda_ty = lambda_ty_elem list * ty
and lambda_arg = {
          la_location     : Location.t;
          la_kind         : Syntax.lvar_kind;
          la_name         : string;
          la_default      : Syntax.expr option;
}
and lambda_args = lambda_arg list
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
and instance = {
          i_hash          : int;
          i_class         : klass specialized;
          i_slots         : value Table.t;
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

exception Exc of exc

(* These tables are safe to use in presence of key mutation. *)
module Valuetbl : Hashtbl.S with type key = value
module EnvTytbl : Hashtbl.S with type key = local_env_ty

(* Types and values *)

val type_of_value       : value -> ty
val type_of_environment : ?imm:bool -> local_env -> local_env_ty

val klass_of_type   : ?dispatch:bool -> ty -> klass
val klass_of_value  : ?dispatch:bool -> ?meta:bool -> value -> klass

val lambda_args_of_formal_args
                    : Syntax.formal_args -> lambda_args
val tys_of_lambda_ty_elems
                    : lambda_ty_elem list -> ty list

(* Correctly handles cyclic structures. *)
val equal           : value -> value -> bool
val hash            : value -> int

(* kawaii operator *)
val (=^-^=)         : value -> value -> bool

val inspect         : value -> string
val inspect_value   : value -> string
val inspect_type    : ty    -> string

val print           : value -> unit
val print_value     : value -> unit
val print_type      : ty    -> unit

(* Virtual image *)

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
  kFixed            : klass;
  kOption           : klass;
  kTuple            : klass;
  kRecord           : klass;
  kLambda           : klass;
  kMixin            : klass;
  kPackage          : klass;
  kMemory           : klass;

  pToplevel         : package;
}

val roots           : roots ref

val create_class    : unit -> klass * klass
val create_roots    : unit -> roots

val adopt_tvar      : int -> tvar

val new_tvar        : unit -> tvar
val new_static_tvar : unit -> tvar

val new_class       : ?ancestor:klass ->
                          ?parameters:tvar Assoc.sequental_t ->
                          string -> klass
val new_package     : string -> package

(* Exceptions *)

val exc_fail        : string -> Location.t list -> 'a
val exc_type        : string -> value -> Location.t list -> 'a

(* Local environments *)

val lenv_create     : local_env option -> local_env
val lenv_bind       : local_env -> string ->
                          kind:Syntax.lvar_kind -> value:value ->
                          loc:Location.t -> unit
val lenv_mutate     : local_env -> string -> value:value -> unit
val lenv_lookup     : local_env -> string -> value

(* Type environments *)

val tenv_create     : unit -> type_env
val tenv_fork       : type_env -> type_env
val tenv_resolve    : type_env -> string -> tvar

(* Constant environments *)

exception CEnvUnbound
exception CEnvAlreadyBound of value

val cenv_empty      : const_env
val cenv_extend     : const_env -> package -> const_env
val cenv_bind       : const_env -> string -> value -> unit
val cenv_peek       : const_env -> string -> value option
val cenv_lookup     : const_env -> string -> value
