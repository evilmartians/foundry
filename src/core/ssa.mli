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
| InvalidInstr
(* Functions *)
| Function        of func
| Argument
| BasicBlock      of basic_block
(* Constants *)
| Const           of Rt.value
(* Phi *)
| PhiInstr        of ((*basic_block*) name * (*value*) name) list
(* Terminators *)
| JumpInstr       of (*target*) name
| JumpIfInstr     of (*condition*) name * (*if_true*) name * (*if_false*) name
| ReturnInstr     of (*value*) name
(* Language-specific opcodes *)
| FrameInstr      of (*parent*) name
| LVarLoadInstr   of (*environment*) name * (*var*) string
| LVarStoreInstr  of (*environment*) name * (*var*) string * (*value*) name
| CallInstr       of (*func*) name   * (*operands*) name list
| PrimitiveInstr  of (*name*) string * (*operands*) name list

(* Generic *)

val name_of_value       : Rt.value -> name
val set_name_id         : name -> string -> unit

(* Module level *)

val name_of_lambda      : ?id:string -> Rt.lambda -> name

(* Function level *)

val create_func         : ?id:string ->
                            ?arg_names:string list ->
                            (*args_ty*)   Rt.ty list ->
                            (*result_ty*) Rt.ty ->
                            name
val func_of_name        : name -> func

val find_func_entry     : (*func*) name -> name

(* Basic block level *)

val create_basic_block  : ?id:string -> (*func*) name -> name
val remove_basic_block  : (*basic_block*) name -> unit
val basic_block_of_name : (*basic_block*) name -> basic_block

val successors          : (*basic_block*) name -> name list
val predecessors        : (*basic_block*) name -> name list

(* Instruction level *)

val append_instr         : ?id:string -> ty:Rt.ty -> opcode:opcode -> (*basic_block*) name -> name
val replace_instr        : ?ty:Rt.ty -> ?opcode:opcode -> (*insn*) name -> unit
val remove_instr         : (*insn*) name -> unit
