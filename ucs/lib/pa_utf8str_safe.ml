open Camlp4

module Id = struct
  let name = "pa_utf8str_safe"
  let version = "1.0"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  EXTEND Gram
    GLOBAL: expr;

    expr: LEVEL "simple"
      [ [ "u"; s = STRING ->
            <:expr< (Unicode.assert_utf8s $str:s$) >>
      ] ]
    ;
  END
end

let module M = Register.OCamlSyntaxExtension(Id) (Make) in ()
