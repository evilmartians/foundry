open Unicode.Std
open Ssa

let run_on_capsule capsule =
  let worklist = Worklist.create () in
  let seen     = ref [] in

  (* Traverse the capsule starting at the set of root functions;
     usually, functions which will become external symbols. *)
  Worklist.put worklist (find_func capsule "main");

  while Worklist.some worklist do
    let funcn = Worklist.take worklist in
    seen := funcn :: !seen;

    (* Find all opcodes which refer to other functions, and mark
       the functions as live. *)
    iter_instrs funcn ~f:(fun instr ->
      List.iter (fun operand ->
        match operand with
        | { opcode = Function _ }
        -> (if not (List.memq operand !seen) then
              Worklist.put worklist operand)
        | _
        -> ()) (instr_operands instr))
  done;

  let seen = !seen in
  iter_funcs capsule ~f:(fun funcn ->
    if not (List.memq funcn seen) then
      remove_func capsule funcn)
