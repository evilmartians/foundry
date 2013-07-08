open Ocamlbuild_plugin;;

dispatch begin function
  | After_rules ->
    (* Compile foundryWeb.js from bytecode foundryWeb.byte *)
    rule "js_of_ocaml: byte -> js"
      ~deps:["%.byte"]
      ~prod:"%.js"
      begin fun env build ->
        Seq [Cmd (S[A"js_of_ocaml"; Px(env "%.byte")])]
      end;

    (* Build dependencies in the source tree *)
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
    dep ["ocaml"; "ocamldep"; "use_utf8str"] ["ucs/lib/pa_utf8str.cmo"];
    dep ["ocaml"; "menhir";   "use_utf8str_safe"] ["ucs/lib/pa_utf8str_safe.cmo"];
    dep ["ocaml"; "ocamldep"; "use_utf8str_safe"] ["ucs/lib/pa_utf8str_safe.cmo"];

  | _ -> ()
end;;
