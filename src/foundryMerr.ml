open Unicode.Std

let () =
  let lexbuf   = Ulexing.from_utf8_channel stdin in
  let lexstate = Lexer.create "stdin" 1 in
  let lex ()   = Lexer.next lexstate lexbuf in
  let parse    = MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel in

  try
    ignore (parse lex)
  with Parser.StateError (token, state) ->
    print_endline ("(" ^ (string_of_int state) ^ ", " ^
                   (Unicode.assert_utf8s (Parser_desc.name_of_token token)) ^ ")")
