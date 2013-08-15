open Unicode.Std

let () =
  let chunk = ByteArray.create 4096 in
  let rec read_some buf =
    let len = input stdin chunk 0 4096 in
      if len = 0 then buf
      else read_some Pervasives.(buf ^ (ByteArray.sub chunk 0 len))
  in

  let buf      = read_some (ByteArray.create 0) in
  let lexbuf   = Ulexing.from_utf8_string buf in
  let lexstate = Lexer.create (Location.register "<stdin>" 1 (Unicode.adopt_utf8s buf)) in
  let lex ()   = Lexer.next lexstate lexbuf in
  let parse    = MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel in

  try
    ignore (parse lex)
  with Parser.StateError (token, state) ->
    print_endline ("(" ^ (string_of_int state) ^ ", " ^
                   (Unicode.assert_utf8s (Parser_desc.name_of_token token)) ^ ")")
