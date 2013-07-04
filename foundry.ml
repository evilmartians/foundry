open Core.Std

let env = Vm.create_env ()

while true do
  let lexbuf = (Lexing.from_channel stdin) in
  let lex    = Lexer.next (Lexer.create ()) in

(*   lex lexbuf
    |> sexp_of_token
    |> Sexp.to_string_hum
    |> print_endline

  let print_stmt stmt =
    stmt
    |> Syntax.sexp_of_expr
    |> Sexp.to_string_hum
    |> print_endline
  in List.iter (Parser.toplevel lex lexbuf) ~f:print_stmt
*)

  try
    let eval expr =
      Vm.eval env expr
      |> Vm.inspect
      |> print_endline
    in List.iter ~f:eval (Parser.toplevel lex lexbuf)
  with Vm.Exc exc ->
    let pointers =
      let all_ranges = exc.Vm.ex_location :: exc.Vm.ex_highlights in
        String.make (List.fold ~f:max ~init:0 (List.map all_ranges ~f:snd) + 1) ' '
    in
      (let x, y = exc.Vm.ex_location in
        String.fill pointers x (y - x) '^');
      List.iter ~f:(fun (x, y) -> String.fill pointers x (y - x) '~') exc.Vm.ex_highlights;
      print_endline pointers;
      print_endline ("Error: " ^ exc.Vm.ex_message)
done
