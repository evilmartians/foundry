open Ssa

let has_side_effects instr =
  match instr.opcode with
  | FrameInstr _ | LVarLoadInstr _
  -> false
  | PrimitiveInstr (name, operands)
  -> Primitive.has_side_effects name
  | _
  -> true

let run_on_function func =
  iter_instrs (fun instr ->
      if instr.uses = [] && not (has_side_effects instr) then
        erase_instr instr)
    func

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
