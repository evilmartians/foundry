open Unicode.Std

val name : string
val run_on_function : Pass_manager.t -> Ssa.capsule -> Ssa.name -> unit
