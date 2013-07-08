open Unicode.Std

type state

val create : string -> int -> state
val next   : state -> Lexing.lexbuf -> Parser.token

exception Unexpected of string * Location.t
