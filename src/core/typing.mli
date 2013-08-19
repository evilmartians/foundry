val match_ty  : (*pattern*) Rt.ty -> (*subject*) Rt.ty ->
                    (Rt.tvar * Rt.ty) list

val rewrite   : (Rt.tvar * Rt.ty) list -> Rt.ty -> Rt.ty
