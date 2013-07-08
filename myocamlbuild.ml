open Ocamlbuild_plugin;;

Command.setup_virtual_command_solver "MENHIR"
  (fun () -> P(Sys.getcwd () ^ "/../menhir-bin/bin/menhir"));;

dispatch begin function
  | After_rules ->
    (* Compile foundryWeb.js from bytecode foundryWeb.byte *)
    rule "js_of_ocaml: byte -> js"
      ~deps:["%.byte"]
      ~prod:"%.js"
      begin fun env build ->
        Seq [Cmd (S[A"js_of_ocaml"; A"-pretty"; A"-noinline"; Px(env "%.byte")])]
      end;

    (* === UNICODE === *)

    (* Build the library *)
    ocaml_lib "ucs/src/ucs";

    (* Add pa_utf8str{,safe}.cmo to the ocaml pre-processor
       when use_utf8str{,safe} is set *)
    flag ["ocaml";"compile"; "use_utf8str"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str.cmo"]);
    flag ["ocaml";"ocamldep";"use_utf8str"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str.cmo"]);

    flag ["ocaml";"compile"; "use_utf8str_safe"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str_safe.cmo"]);
    flag ["ocaml";"ocamldep";"use_utf8str_safe"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str_safe.cmo"]);

    (* Running ocamldep on ocaml code that is tagged with use_utf8str{,safe}
       will require the cmo. Note that you only need this declaration when the
       syntax extension is part of the sources to be compiled with ocamlbuild. *)
    dep ["ocaml"; "ocamldep"; "use_utf8str"]      ["ucs/lib/pa_utf8str.cmo"];
    dep ["ocaml"; "menhir";   "use_utf8str_safe"] ["ucs/lib/pa_utf8str_safe.cmo"];
    dep ["ocaml"; "ocamldep"; "use_utf8str_safe"] ["ucs/lib/pa_utf8str_safe.cmo"];

    (* === MERR === *)

    rule "merr: mly.in -> mly"
      ~deps:["%.mly.in"]
      ~prod:"%.mly"
      begin fun env build ->
        Seq [Cmd (S[ A"sed"; A"-e"; A"s/^\\(%token[^\"]*\\w\\+\\)\\s*\".*\"$/\\1/";
                     Px(env "%.mly.in"); Sh">"; Px(env "%.mly") ])]
      end;

    rule "merr: terminals.mly.in -> tokens.ml, tokens.mli"
      ~deps:["%_terminals.mly"]
      ~prods:["%_tokens.ml"; "%_tokens.mli"]
      begin fun env build ->
        Seq [Cmd (S[ V"MENHIR"; A"--only-tokens"; A"-b"; Px(env "%_tokens");
                     Px(env "%_terminals.mly") ])]
      end;

    rule "merr: mly -> ml"
      ~deps:["%_nonterminals.mly"; "%_terminals.mly"]
      ~prods:["%_parser.ml"; "%_parser.mli"]
      begin fun env build ->
        Seq [Cmd (S[ V"MENHIR"; A"--external-tokens"; Px("E_tokens");
                     Px(env "%_nonterminals.mly");
                     Px(env "%_terminals.mly");
                     A"-b"; Px(env "%_parser") ])]
      end

  | _ -> ()
end;;
