open Unicode.Std

type name = private {
  mutable id     : string;
  mutable ty     : Rt.ty;
  mutable opcode : opcode;

  (* Internal fields *)
  mutable parent : name option;
  mutable uses   : name list;
}
and func = private {
          naming    : func_naming;
  mutable arguments : name list;
}
and func_naming
and basic_block = private {
  mutable instructions : name list;
}
and opcode =
(* Functions *)
| Function   of func
| Argument
| BasicBlock of basic_block
(* Phi *)
| Phi        of (name * name) list
(* Terminators *)
| Jump       of name
| JumpIf     of name * name * name
| Return     of name
(* Language-specific opcodes *)
| Const      of Rt.value
| Primitive0 of string
| Primitive1 of string * name
| Primitive2 of string * name * name

val create_func        : ?id:string -> Rt.ty list -> Rt.ty -> name
val create_basic_block : ?id:string -> parent:name -> name

val func_of_lambda     : ?id:string -> Rt.lambda -> name
