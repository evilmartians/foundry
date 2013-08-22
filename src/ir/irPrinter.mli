open Unicode.Std

val ordered   : bool ref

val print_name  : Ssa.name -> unit

val string_of   : ?omit_roots:bool -> Rt.roots -> Ssa.capsule -> string
