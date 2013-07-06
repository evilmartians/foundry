type diagnostic = string * Location.t * Location.t list

type result =
  | Output      of string
  | Diagnostics of diagnostic list
  | Error       of string

let eval str =
  let env    = Vm.env_create () in
  let lexbuf = (Lexing.from_string str) in
  let lex    = Lexer.next (Lexer.create ()) in

  try
    Output (Vm.inspect (Vm.eval env (Parser.toplevel lex lexbuf)))
  with
  | Vm.Exc exc ->
    Diagnostics [exc.Vm.ex_message, exc.Vm.ex_location, exc.Vm.ex_highlights]
  | Parser.Error ->
    Error "Cannot parse"
  | Failure msg ->
    Error msg

let js_eval input =
  let inject = Js.Unsafe.inject
  in
  let return ty value =
    Js.Unsafe.obj [|
      ("type",  inject (Js.string ty));
      ("value", inject value)
    |]
  in
  let input = Js.to_string input in
    match eval input with
    | Output value
    -> return "output" (Js.string value)

    | Diagnostics lst
    -> (let js_of_loc loc =
          Js.array [| (fst loc); (snd loc) |]
        in let diags =
          Js.array (Array.of_list (List.map
            (fun (msg, loc, hilights) ->
              let msg = Js.string msg in
              let loc = js_of_loc loc in
              let hilights =
                Js.array (Array.of_list (List.map js_of_loc hilights))
              in Js.array [| inject msg; inject loc; inject hilights |])
            lst))
        in return "diagnostics" diags)

    | Error desc
    -> return "error" (Js.string desc)

let _ =
  (Js.Unsafe.coerce Dom_html.window)##foundryEval <- Js.wrap_callback js_eval
