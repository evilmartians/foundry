open Unicode.Std

type t

module type FunctionPass =
sig
  val name            : string
  val run_on_function : t -> Ssa.capsule -> Ssa.name -> unit
end

module type CapsulePass =
sig
  val name            : string
  val run_on_capsule  : t -> Ssa.capsule -> unit
end

val create            : sequental:bool -> t
val add_function_pass : t -> (module FunctionPass) -> unit
val add_capsule_pass  : t -> (module CapsulePass)  -> unit
val add_pass_manager  : t -> t -> unit

val verbose : bool ref

val run     : t -> Ssa.capsule -> unit
val mark    : t -> ?reason:string -> (*func*) Ssa.name -> unit
