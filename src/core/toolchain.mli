open Unicode.Std

type 'a result =
| Success of 'a
| Failure of Diagnostic.t list

val input_file : latin1s -> Location.file * Ulexing.lexbuf

val parse_ir : Lexing.lexbuf -> Rt.roots * Ssa.capsule
val print_ir : ?omit_roots:bool -> Rt.roots * Ssa.capsule -> string

val parse_source       : Location.file * Ulexing.lexbuf -> Syntax.exprs result
val eval_ast           : Vm.env -> Syntax.exprs -> Rt.value result
val parse_eval_source  : Vm.env -> Location.file * Ulexing.lexbuf -> Rt.value result

val bootstrap_capsule  : Rt.roots -> Ssa.capsule
val build_pass_manager : unit -> Pass_manager.t
