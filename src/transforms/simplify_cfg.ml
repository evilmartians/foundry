open Unicode.Std
open Ssa

let run_on_function func =
  let worklist = Worklist.create () in
  (* Populate worklist with all basic blocks in the function. *)
  iter_blocks (Worklist.put worklist) func;

  while Worklist.some worklist do
    let blockn = Worklist.take worklist in

    if (List.length (successors blockn)) = 1 then begin
      (* If the current block has a single successor, and that
         successor has only a single predecessor, merge it into
         the current block. *)
      let succn = List.hd (successors blockn) in
      if (List.length (predecessors succn)) = 1 then begin
        (* Remove terminator of the current block. *)
        erase_instr (terminator blockn);

        (* Append all instructions of dominated block to the
           current one. *)
        iter_instrs (fun instr ->
            remove_instr instr;
            append_instr instr blockn)
          succn;

        (* Remove now-empty dominated block. *)
        remove_block succn;
        Worklist.remove worklist succn;

        (* This transformation might have exposed another
           opportunities, re-check current block. *)
        Worklist.put worklist blockn
      end
    end;

    if (predecessors blockn) = [] && (func_entry func) != blockn then begin
      (* This block does not have any predecessors and is not the
         entry block. Remove it, as it's dead. *)
      remove_block blockn;
      Worklist.remove worklist blockn
    end
  done

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
