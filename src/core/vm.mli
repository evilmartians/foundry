open Unicode.Std

type env
val env_create : unit -> env

val eval : env -> Syntax.exprs -> Rt.value
