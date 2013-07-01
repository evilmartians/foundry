open Sexplib.Std

let parse str =
  let lexbuf = (Lexing.from_string str)  in
  let lex    = Lexer.next (Lexer.create ()) in

  try
    let ast = Parser.compstmt lex lexbuf in
      let sexp = Syntax.sexp_of_expr ast in
        Sexplib.Sexp.to_string_hum sexp ^ "\n"
  with Parser.Error ->
    "Parse error\n"

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
  textbox##value <- Js.string "2 + 2";
  Dom.appendChild top textbox;
  Dom.appendChild top (Html.createBr doc);

  textbox##focus(); textbox##select();
  let parse_btn =
    button "Parse"
      (fun () ->
         append (parse (Js.to_string textbox##value));
         textbox##focus(); textbox##select();
         doc##documentElement##scrollTop <- doc##body##scrollHeight)
  in
  Dom.appendChild top parse_btn;

  Js._false

let _ = Html.window##onload <- Html.handler run
