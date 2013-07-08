open Unicode.Std

type state

val create : string -> int -> state
val next   : state -> Ulexing.lexbuf -> (Parser.token * Lexing.position * Lexing.position)

exception Unexpected of string * Location.t
