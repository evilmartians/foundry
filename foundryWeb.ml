type diagnostic = string * Location.t list

type result =
  | Output      of string
  | Diagnostics of diagnostic list
  | Error       of string

let eval str =
  let env    = Vm.env_create () in
  let lexbuf = (Lexing.from_string str) in
  let lex    = Lexer.next (Lexer.create "input" 1) in

  try
    Output (Vm.inspect (Vm.eval env (Parser.toplevel lex lexbuf)))
  with
  | Vm.Exc exc ->
    Diagnostics [exc.Vm.ex_message, exc.Vm.ex_locations]
  | Parser.Error ->
    Error "Cannot parse"
  | Failure msg ->
    Error msg

let js_eval input =
  let inject = Js.Unsafe.inject
  in
  let array f lst =
    Array.of_list (List.map f lst)
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
          let f, p1, p2 = Location.unpack loc in
            Js.Unsafe.obj [|
              ("file", inject (Js.string f));
              ("from", inject p1);
              ("to",   inject p2)
            |]
        in let diags =
          Js.array (array
            (fun (message, locations) ->
              let message = Js.string message in
              let locations = Js.array (array js_of_loc locations) in
                Js.Unsafe.obj [|
                  ("message",   inject message);
                  ("locations", inject locations)
                |])
            lst)
        in return "diagnostics" diags)

    | Error desc
    -> return "error" (Js.string desc)

let _ =
  (Js.Unsafe.coerce Dom_html.window)##foundryEval <- Js.wrap_callback js_eval
