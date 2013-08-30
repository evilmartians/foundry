open Unicode.Std
open Ssa

let name = "Dead Code Elimination"

let has_side_effects instr =
  match instr.opcode with
  | FrameInstr _ | LVarLoadInstr _ | ClosureInstr _
  | PhiInstr _
  -> false
  | PrimitiveInstr (name, operands)
  -> Primitive.has_side_effects name
  | _
  -> true

let run_on_function passmgr capsule funcn =
  let worklist = Worklist.create () in
  (* Populate worklist with all instructions in the function. *)
  iter_instrs (Worklist.put worklist) funcn;

  while Worklist.some worklist do
    let instr = Worklist.take worklist in
    (* If an instruction does not have side effects and is not
       referenced, erase it and add its operands to worklist. *)
    if instr.n_uses = [] && not (has_side_effects instr) then begin
      Worklist.append worklist (instr_operands instr);
      Worklist.remove worklist instr;
      erase_instr instr
    end
  done
