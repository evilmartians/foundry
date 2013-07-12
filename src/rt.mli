open Unicode.Std

type tvar
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
  l_location      : Location.t;
  l_ty            : value;
  l_local_env     : local_env;
  l_type_env      : type_env;
  l_const_env     : const_env;
  l_args          : Syntax.formal_args;
  l_code          : Syntax.exprs;
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
  ex_locations    : Location.t list;
}
with sexp_of

exception Exc of exc
with sexp

val exc_fail      : string -> Location.t list -> 'a
val exc_type      : string -> value -> Location.t list -> 'a

val typeof        : value -> value
val klassof       : value -> klass

val inspect_value : value -> string
val inspect_type  : value -> string
val inspect       : value -> string

val genvar        : unit -> tvar

type roots = {
  kClass          : klass;
  kTypeVariable   : klass;
  kNil            : klass;
  kBoolean        : klass;
  kInteger        : klass;
  kSymbol         : klass;
  kTuple          : klass;
  kRecord         : klass;
  kLambda         : klass;
  kMixin          : klass;
  kPackage        : klass;

  pToplevel       : package;
}

val roots         : roots ref

val reset         : unit -> unit

val new_class     : ?ancestor:klass -> string -> klass
val new_package   : string -> package
