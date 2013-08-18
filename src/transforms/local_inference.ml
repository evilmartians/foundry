open Unicode.Std
open Ssa

let run_on_function func =
  iter_blocks func ~f:(fun blockn ->
    match (terminator blockn).opcode with
    | ReturnInstr value
    -> (match func.ty with
        | Rt.FunctionTy (_, return_ty)
        -> (* Find all return instructions whose type differs
              from the return type of function. With a hypothesus
              that the instruction type is more specific, narrow
              the function type. *)
           (if value.ty <> return_ty then
              let func_ty' = Typing.func_specialize ~return_ty:value.ty func.ty
              in set_ty func_ty' func)
        | _
        -> ())
    | _
    -> ())

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
