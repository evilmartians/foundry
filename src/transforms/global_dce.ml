open Unicode.Std
open Ssa

let run_on_capsule capsule =
  let worklist = Worklist.create () in
  let seen     = ref [] in
  Worklist.put worklist (find_func capsule "main");

  while Worklist.some worklist do
    let funcn = Worklist.take worklist in
    seen := funcn :: !seen;
    iter_instrs funcn ~f:(fun instr ->
      match instr.opcode with
      | CallInstr (funcn, _)
      | MakeClosureInstr (funcn, _)
      -> (if not (List.memq funcn !seen) then
            Worklist.put worklist funcn)
      | _
      -> ())
  done;

  let seen = !seen in
  iter_funcs capsule ~f:(fun funcn ->
    if not (List.memq funcn seen) then
      remove_func capsule funcn)