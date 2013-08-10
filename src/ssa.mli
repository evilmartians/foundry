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
          naming       : func_naming;
  mutable arguments    : name list;
  mutable basic_blocks : name list;
}
and func_naming
and basic_block = private {
  mutable instructions : name list;
}
and opcode =
(* Functions *)
| Function        of func
| Argument
| BasicBlock      of basic_block
(* Constants *)
| Const           of Rt.value
(* Phi *)
| PhiInsn         of (name * name) list
(* Terminators *)
| JumpInsn        of name
| JumpIfInsn      of name * name * name
| ReturnInsn      of name
(* Language-specific opcodes *)
| EnvironmentInsn of Rt.bindings_ty * (*parent*) name
| LVarLoadInsn    of name * string
| LVarStoreInsn   of name * string * name
| Primitive0Insn  of string
| Primitive1Insn  of string * name
| Primitive2Insn  of string * name * name

(* Module level *)

val name_of_lambda      : ?id:string -> Rt.lambda -> name

(* Function level *)

val create_func         : ?id:string -> Rt.ty list -> Rt.ty -> name
val create_basic_block  : ?id:string -> parent:name -> name

val func_of_name        : name -> func
val basic_block_of_name : name -> basic_block

val entry               : name -> name
val add_basic_block     : (*func*) name -> (*basic_block*) name -> unit
val remove_basic_block  : (*basic_block*) name -> unit

(* Instruction level *)

val const               : Rt.value -> name

val append_insn         : (*basic_block*) name -> ?id:string -> Rt.ty -> opcode -> name
val replace_insn        : (*insn*) name -> ?id:string -> Rt.ty -> opcode -> unit
val remove_insn         : (*insn*) name -> unit
