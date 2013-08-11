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
        Seq [
          Cmd (S[A"js_of_ocaml"; A"-pretty"; A"-noinline"; A"-debuginfo";
               P(env "%.byte"); A"-o"; P(env "%.js.o")]);
          Cmd (S[A"mv"; P(env "%.js.o"); Px(env "%.js")])
        ]
      end;

    (* === UNICODE === *)

    (* Add pa_utf8str.cmo to the ocaml pre-processor when use_utf8str is set *)
    flag ["ocaml"; "compile"; "use_utf8str"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str.cmo"]);
    flag ["ocaml"; "ocamldep"; "use_utf8str"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str.cmo"]);
    flag ["ocaml"; "infer_interface"; "use_utf8str"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str.cmo"]);

    flag ["ocaml"; "compile"; "use_utf8str_safe"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str_safe.cmo"]);
    flag ["ocaml"; "ocamldep"; "use_utf8str_safe"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str_safe.cmo"]);
    flag ["ocaml"; "infer_interface"; "use_utf8str_safe"] (S[A"-ppopt"; A"ucs/lib/pa_utf8str_safe.cmo"]);

    (* Running ocamldep on ocaml code that is tagged with use_utf8str
       will require the cmo. Note that you only need this declaration when the
       syntax extension is part of the sources to be compiled with ocamlbuild. *)
    dep ["ocaml"; "ocamldep"; "use_utf8str"] ["ucs/lib/pa_utf8str.cmo"];
    dep ["ocaml"; "ocamldep"; "use_utf8str_safe"] ["ucs/lib/pa_utf8str_safe.cmo"];

    (* === MENHIR AND MERR == *)

    ocaml_lib "merr/_build/libmerr/libmerr";

    (* These two rules allow to separate the parser into two parts: terminals
       and nonterminals. This way, the lexer does not depend on entire parser,
       but only on the module with token definitions. *)

    rule "menhir: terminals.mly -> tokens.ml, tokens.mli"
      ~deps:["%_terminals.mly"]
      ~prods:["%_tokens.ml"; "%_tokens.mli"]
      begin fun env build ->
        Seq [Cmd (S[ V"MENHIR"; A"--only-tokens"; A"-b"; Px(env "%_tokens");
                     Px(env "%_terminals.mly") ])]
      end;

    (* Pass module name as an argument:
       "merr/e_parser.mlypack":external_tokens(E_tokens) *)

    pflag ["ocaml"; "menhir"] "external_tokens" (fun ml -> S[A"--external-tokens"; A ml]);

    (* Generate the automaton description file, which merr uses as the source. *)
    flag ["ocaml"; "menhir"; "dump"] (A"--dump");

    (* Generate merr error description file. *)
    rule "merr: errors.ml.in -> errors.ml"
      ~prod:"%_errors.ml"
      ~deps:[
        "%_errors.ml.in";
        "%_terminals.mly";
        "%.ml";
        "src/foundry_merr.native"
      ]
      begin fun env build ->
        Cmd(S[
          P "../merr/merr.native";
          A"-p"; A("src/foundry_merr.native");
          A"-t"; P(env "%_terminals.mly");
          A"-a"; P(env "%.automaton");
          A"-e"; P(env "%_errors.ml.in");
          A"-o"; Px(env "%_errors.ml");
        ]);
      end

  | _ -> ()
end;;
