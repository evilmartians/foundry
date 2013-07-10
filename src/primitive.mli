open Unicode.Std

val exists : string (* name *) -> int (* arity *) -> bool

val invoke : string -> Rt.value list -> Rt.value
