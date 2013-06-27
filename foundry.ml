open Core.Std

let lexbuf = (Lexing.from_channel stdin)
let lex    = Lexer.next (Lexer.create ())

while true do
(*   lex lexbuf
    |> sexp_of_token
    |> Sexp.to_string_hum
    |> print_endline
 *)

  Parser.compstmt lex lexbuf
    |> Syntax.sexp_of_expr
    |> Sexp.to_string_hum
    |> print_endline
done
