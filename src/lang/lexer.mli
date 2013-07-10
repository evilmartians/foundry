open Unicode.Std

type state

val create : string -> int -> state
val next   : state -> Ulexing.lexbuf -> (Parser_tokens.token * Lexing.position * Lexing.position)

exception Unexpected of Location.t * char
