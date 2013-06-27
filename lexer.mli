type state

val create : unit -> state
val next : state -> Lexing.lexbuf -> Parser.token
