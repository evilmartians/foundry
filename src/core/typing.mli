open Rt

exception Conflict of Rt.ty * Rt.ty

val unify     : (*pattern*) ty -> (*subject*) ty -> (tvar * ty) list
val subst     : (tvar * ty) list -> ty -> ty

val print_env : (tvar * ty) list -> unit
