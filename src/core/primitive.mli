open Unicode.Std

val exists            : string -> bool
val has_side_effects  : string -> bool
val invoke            : string -> Rt.value list -> Rt.value
