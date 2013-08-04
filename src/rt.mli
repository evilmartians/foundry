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
| Instance      of klass specialized * slots
and 'a specialized = 'a * value Table.t
and slots = value Table.t
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
  b_kind          : Syntax.lvar_kind;
  b_value         : value;
}
and local_env = {
  e_parent        : local_env option;
  e_bindings      : binding Table.t;
}
and type_env =      tvar Table.t
and const_env =     package list
and lambda = {
  l_location      : Location.t;
  l_ty            : value;
  mutable l_local_env : local_env;
  mutable l_type_env  : type_env;
  mutable l_const_env : const_env;
  l_args          : Syntax.formal_args;
  l_body          : Syntax.exprs;
}
and lambda_ty = {
  l_args_ty       : value;
  l_kwargs_ty     : value;
  l_result_ty     : value;
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
  iv_ty           : value;
}
and exc = {
  ex_message      : string;
  ex_locations    : Location.t list;
}
with sexp_of

exception Exc of exc
with sexp

val exc_fail      : string -> Location.t list -> 'a
val exc_type      : string -> value -> Location.t list -> 'a

val type_of_value  : value -> value
val klass_of_type  : value -> klass
val klass_of_value : ?dispatch:bool -> value -> klass

val inspect_value : value -> string
val inspect_type  : value -> string
val inspect       : value -> string

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

val create_class  : unit -> klass * klass
val create_roots  : unit -> roots

val adopt_tvar    : int -> tvar

val new_tvar      : unit -> tvar
val new_class     : ?ancestor:klass -> string -> klass
val new_package   : string -> package

val roots         : roots ref
