open Unicode.Std
open Fy_big_int

type 'a result =
| Success of 'a
| Failure of Diagnostic.t list

let input_file filename =
  let content = Io.input_file filename in
  let file    = Location.register (Unicode.assert_utf8s filename) 1 content in
  let lexbuf  = Ulexing.from_utf8_string (content :> latin1s) in
  file, lexbuf

let parse_ir lexbuf =
  let lex () = IrLexer.next lexbuf in
  let parse  = MenhirLib.Convert.Simplified.traditional2revised IrParser.toplevel in
  try
    parse lex
  with IrParser.StateError _ ->
    let pos = Lexing.lexeme_start_p lexbuf in
    failwith ("Invalid input near " ^ (string_of_int pos.Lexing.pos_lnum) ^ ":" ^
                   (string_of_int pos.Lexing.pos_bol))

let print_ir ?(omit_roots=false) ir =
  let (roots, capsule) = ir in
  IrPrinter.string_of ~omit_roots roots capsule

let parse_source (file, lexbuf) =
  let lexstate = Lexer.create file in
  let lex () = Lexer.next lexstate lexbuf in
  let parse  = MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel in
  try
    let ast = parse lex in
    match Verifier.check ast with
    | []    -> Success ast
    | diags -> Failure diags
  with
  | Lexer.Unexpected (loc, chr)
  -> (let diag = Diagnostic.Fatal, Unicode.Std.(
                 "Unexpected character " ^ (Char.escaped chr) ^
                 " (" ^ (String.make 1 chr) ^ ")"), [loc] in
      Failure [diag])
  | Parser.StateError (token, state)
  -> (let diag = Diagnostic.Error,
                 Unicode.assert_utf8s (Parser_errors.message state token),
                 [Parser_desc.loc_of_token token] in
      Failure [diag])

let eval_ast env expr =
  try
    Success (Vm.eval env expr)
  with Rt.Exc exc ->
    let diag = Diagnostic.Error, exc.Rt.ex_message, exc.Rt.ex_locations in
    Failure [diag]

let parse_eval_source env source =
  match parse_source source with
  | Success ast
  -> eval_ast env ast
  | Failure _ as result
  -> result

let bootstrap_capsule roots =
  let capsule  = Ssa.create_capsule () in
  let funcn    = Ssa.create_func ~name:"main" [] (Rt.UnsignedTy 32) in
  Ssa.add_func capsule funcn;

  let entry    = Ssa.create_block "entry" in
  Ssa.add_block funcn entry;
  let toplevel = Ssa.const (Rt.Package roots.Rt.pToplevel) in

  let send     = Ssa.create_instr (Rt.tvar_as_ty ())
                    (Ssa.PrimitiveInstr ("obj_send", [
                      toplevel; Ssa.const (Rt.Symbol "main");
                        Ssa.const (Rt.Tuple ([
                            Rt.Package roots.Rt.pToplevel ]));
                        Ssa.const (Rt.Record (
                            Assoc.empty))
                    ])) in
  Ssa.append_instr send entry;

  let return   = Ssa.create_instr (Rt.NilTy)
                    (Ssa.ReturnInstr (
                        Ssa.const (Rt.Unsigned (32, (big_int_of_int 0))))) in
  Ssa.append_instr return entry;

  capsule

let build_pass_manager () =
  let outer = Pass_manager.create ~sequential:true in
    let inner = Pass_manager.create ~sequential:false in
      Pass_manager.add_function_pass inner (module Dead_code_elim);
      Pass_manager.add_function_pass inner (module Constant_folding);
      Pass_manager.add_function_pass inner (module Cfg_simplification);
      Pass_manager.add_function_pass inner (module Local_inference);
      Pass_manager.add_function_pass inner (module Coercion);
      Pass_manager.add_function_pass inner (module Method_resolution);
      Pass_manager.add_function_pass inner (module Specialization);
      Pass_manager.add_function_pass inner (module Local_inference);
    Pass_manager.add_pass_manager outer inner;
    Pass_manager.add_capsule_pass outer (module Global_dce);
    outer
