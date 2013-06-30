open Core.Std

let lexbuf = (Lexing.from_channel stdin)
let lex    = Lexer.next (Lexer.create ())

while true do
(*   lex lexbuf
    |> sexp_of_token
    |> Sexp.to_string_hum
    |> print_endline
 *)

  let print_stmt stmt =
    stmt
    |> Syntax.sexp_of_expr
    |> Sexp.to_string_hum
    |> print_endline
  in ignore (List.map (Parser.toplevel lex lexbuf) ~f:print_stmt)
done
