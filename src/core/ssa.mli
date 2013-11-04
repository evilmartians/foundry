open Unicode.Std

type name = private {
  mutable id        : string;
  mutable ty        : Rt.ty;
  mutable opcode    : opcode;
  mutable location  : Location.t;

  (* Internal fields *)
  mutable n_parent  : name_parent;
  mutable n_uses    : name list;
          n_hash    : int;
}
and name_parent
and capsule = private {
  mutable functions    : name list;
          overloads    : overloads;
          lambda_cache : lambda_cache;

  (* Internal fields *)
          c_symtab     : Symtab.t;
}
and overloads
and lambda_cache
and func = private {
          name         : string;
  mutable arguments    : name list;
  mutable basic_blocks : name list;

  (* Internal fields *)
          f_symtab     : Symtab.t;
  mutable f_original   : name option;
}
and basic_block = private {
  mutable instructions : name list;
}
and opcode =
| InvalidInstr
(* Functions *)
| Function          of func
| Argument
| BasicBlock        of basic_block
(* Constants *)
| Const             of Rt.value
(* Terminators *)
| JumpInstr         of (*target*) name
| JumpIfInstr       of (*condition*) name * (*if_true*) name * (*if_false*) name
| ReturnInstr       of (*value*) name
(* Other *)
| PhiInstr          of ((*basic_block*) name * (*value*) name) list
| SelectInstr       of (*condition*) name * (*if_true*) name * (*if_false*) name
(* Language-specific opcodes *)
| FrameInstr        of (*parent*) name
| LVarLoadInstr     of (*environment*) name * (*name*) string
| LVarStoreInstr    of (*environment*) name * (*name*) string * (*value*) name
| IVarLoadInstr     of (*object*) name  * (*name*) string
| IVarStoreInstr    of (*object*) name  * (*name*) string * (*value*) name
| CallInstr         of (*func*) name    * (*operands*) name list
| ClosureInstr      of (*func*) name    * (*environment*) name
| SpecializeInstr   of (*type*) name    * name Assoc.sorted_t
| TupleExtendInstr  of (*tuple*) name   * (*elems*) name list
| TupleConcatInstr  of (*tuple*) name   * (*tuple*) name
| RecordExtendInstr of (*record*) name  * (*elems*) (name * name) list
| RecordConcatInstr of (*record*) name  * (*record*) name
| PrimitiveInstr    of (*name*) string  * (*operands*) name list

(* Nametbl is safe to use in presence of key mutation. *)
module Nametbl : Hashtbl.S with type key = name

(* Generic *)

exception ConvergenceFailure of Diagnostic.t list
val fail_convergence : string -> Location.t list -> 'a

val const           : ?loc:Location.t -> Rt.value -> name
val set_id          : name -> string -> unit
val set_ty          : name -> Rt.ty  -> unit

val replace_all_uses_with : name -> name -> unit

(* Module level *)

val create_capsule  : unit -> capsule

val find_func       : capsule -> string -> name
val iter_funcs      : f:( (*func*) name -> unit) ->
                          capsule -> unit

val iter_overloads  : f:( (*func*)  name -> Rt.ty -> (*func'*) name -> unit) ->
                        capsule -> unit
val find_overload   : f:( Rt.ty -> (*func'*) name -> int option) ->
                        capsule -> (*func*) name -> (*func'*) name
val add_overload    : capsule -> (*func*) name -> Rt.ty -> (*func'*) name -> unit

val iter_lambdas    : f:(Rt.lambda -> (*func*) name -> unit) ->
                        capsule -> unit
val lookup_lambda   : capsule -> Rt.lambda -> (*func*) name option
val add_lambda      : capsule -> Rt.lambda -> (*func*) name -> unit

(* Function level *)

val create_func     : ?name:string -> ?loc:Location.t ->
                      ?arg_ids:string list -> ?arg_locs:Location.t list ->
                      (*args_ty*)   Rt.ty list ->
                      (*result_ty*) Rt.ty ->
                          name
val func_of_name    : name -> func

val func_ty         : (*func*) name -> Rt.ty list * Rt.ty

val add_func        : capsule -> (*func*) name -> unit
val remove_func     : capsule -> (*func*) name -> unit

val func_entry      : (*func*) name -> name
val iter_args       : f:( (*argument*) name -> unit) ->
                        (*func*) name -> unit
val iter_blocks     : f:( (*basic_block*) name -> unit) ->
                        (*func*) name -> unit

val copy_func       : ?suffix:string -> (*func*) name -> (*func'*) name
val specialize      : (*func*) name -> (Rt.tvar * Rt.ty) list -> bool

(* Basic block level *)

val create_block    : string -> name
val block_of_name   : (*basic_block*) name -> basic_block

val add_block       : (*func*) name -> (*basic_block*) name -> unit
val remove_block    : (*basic_block*) name -> unit

val block_parent    : (*basic_block*) name -> (*func*) name

val iter_instrs     : f:( (*func or basic_block*) name -> unit) ->
                        (*func*) name -> unit
val terminator      : (*basic_block*) name -> name

val successors      : (*basic_block*) name -> name list
val predecessors    : (*basic_block*) name -> name list

(* Instruction level *)

val create_instr    : ?id:string -> ?loc:Location.t -> Rt.ty -> opcode -> name
val prepend_instr   : ?before:name -> (*instr*) name -> (*basic_block*) name -> unit
val append_instr    : ?after:name  -> (*instr*) name -> (*basic_block*) name -> unit
val set_opcode      : (*instr*) name -> opcode -> unit
val replace_instr   : (*instr*) name -> (*instr*) name -> unit
val remove_instr    : (*instr*) name -> unit
val erase_instr     : (*instr*) name -> unit

val instr_operands  : (*instr*) name -> name list
val instr_parent    : (*instr*) name -> (*basic_block*) name
val iter_uses       : f:( (*instr*) name -> unit) ->
                          (*instr*) name -> unit
