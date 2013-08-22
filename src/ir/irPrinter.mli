open Unicode.Std

val ordered   : bool ref

val print_name  : Ssa.name -> unit

val string_of   : ?roots:Rt.roots -> Ssa.capsule -> string
