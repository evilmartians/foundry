open Unicode.Std
open Ssa

let run_on_function funcn =
  ()

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
