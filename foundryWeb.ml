open Sexplib.Std

let eval str =
  let env = Vm.env_create () in
  let lexbuf = (Lexing.from_string str) in
  let lex    = Lexer.next (Lexer.create ()) in
  let (|>) x f = f x in

  try
    (List.map (Vm.eval env) (Parser.toplevel lex lexbuf)
    |> List.rev
    |> List.hd
    |> Vm.inspect) ^ "\n"
  with Vm.Exc exc ->
    let pointers =
      let all_ranges = exc.Vm.ex_location :: exc.Vm.ex_highlights in
        String.make (List.fold_left max 0 (List.map snd all_ranges) + 1) ' '
    in
      List.iter (fun (x, y) -> String.fill pointers x (y - x) '~') exc.Vm.ex_highlights;
      (let x, y = exc.Vm.ex_location in
        String.fill pointers x (y - x) '^');

      str ^ "\n" ^ pointers ^ "\nError: " ^ exc.Vm.ex_message ^ "\n"
  with Parser.Error ->
    "Parsing error"
  with Failure msg ->
    "Runtime failure: " ^ msg

module Html = Dom_html
let doc = Html.document

let button_type = Js.string "button"
let button txt action =
  let b = Dom_html.createInput ~_type:button_type doc in
  b##value <- Js.string txt;
  b##onclick <- Dom_html.handler (fun _ -> action (); Js._true);
  b

let run _ =
  let top =
    Js.Opt.get (doc##getElementById(Js.string "foundry"))
      (fun () -> assert false) in
  let output = Html.createDiv doc in
  output##id <- Js.string "output";
  output##style##whiteSpace <- Js.string "pre";
  output##style##fontFamily <- Js.string "monospace";
  Dom.appendChild top output;

  let append str =
    Dom.appendChild output
      (doc##createTextNode(Js.string str));
  in

  let textbox = Html.createTextarea doc in
  textbox##rows <- 10; textbox##cols <- 80;
  textbox##value <- Js.string "let x = [1,2]; let [y] = x";
  Dom.appendChild top textbox;
  Dom.appendChild top (Html.createBr doc);

  textbox##focus(); textbox##select();
  let parse_btn =
    button "Execute"
      (fun () ->
         append (eval (Js.to_string textbox##value));
         textbox##focus(); textbox##select();
         doc##documentElement##scrollTop <- doc##body##scrollHeight)
  in
  Dom.appendChild top parse_btn;

  Js._false

let _ = Html.window##onload <- Html.handler run
