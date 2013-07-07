open Ocamlbuild_plugin;;

dispatch begin function
  | Before_rules ->
    rule "js_of_ocaml: byte -> js"
      ~deps:["%.byte"]
      ~prod:"%.js"
      begin fun env build ->
        Seq [Cmd (S[A"js_of_ocaml"; Px(env "%.byte")])]
      end;
    flag ["ocaml"; "byte"; "compile"] (S[A"-I"; P"lib"]);
  | _ -> ()
end;;
