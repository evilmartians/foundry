val unify : (*pattern*) Rt.ty -> (*subject*) Rt.ty ->
              (Rt.tvar * Rt.ty) list

val subst : (Rt.tvar * Rt.ty) list -> Rt.ty -> Rt.ty
