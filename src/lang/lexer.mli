open Unicode.Std

exception Unexpected of Location.t * char

type state

val create : Location.file -> state
val next   : state -> Ulexing.lexbuf -> (Parser_tokens.token * Lexing.position * Lexing.position)
