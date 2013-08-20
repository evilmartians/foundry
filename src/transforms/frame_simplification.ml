open Unicode.Std
open Ssa

let run_on_function funcn =
  let rec lookup env name =
    match Table.get env.Rt.e_bindings name with
    | Some binding -> binding
    | None -> lookup (Option.get env.Rt.e_parent) name
  in
  iter_instrs funcn ~f:(fun instr ->
    (* Find all stack frames in this function. Usually
       there would be only one, as that's how the frontend
       generates code; however, closure conversion may split
       additional frames. *)
    match instr.opcode with
    | FrameInstr { opcode = Const (Rt.Environment env) }
    -> (iter_uses instr ~f:(fun frame_user ->
          (* Find dependent loads. *)
          match frame_user.opcode with
          | LVarLoadInstr (_, name)
          -> (let binding = lookup env name in
              (* If the load refers to an immutable slot, replace
                 it with the value itself. *)
              if binding.Rt.b_kind = Syntax.LVarImmutable then
                replace_instr frame_user
                  (name_of_value binding.Rt.b_value))
          | _
          -> ()))
    | _
    -> ())

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
