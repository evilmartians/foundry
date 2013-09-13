open Unicode.Std
open Rt

exception Conflict of ty * ty

val unify         : ty -> ty -> (tvar * ty) list
val unify_list    : ty list  -> (tvar * ty) list
val subst         : (tvar * ty) list -> ty -> ty
val meaningful    : (tvar * ty) list -> (tvar * ty) list

val fold_equiv    : ty -> ty
val unfold_equiv  : ty -> (klass specialized)
val equiv         : f:(klass specialized -> ty) -> ty -> ty

val instantiate   : ty -> ty

val slot_ty       : klass specialized -> string -> ty

val print_env     : (tvar * ty) list -> unit
