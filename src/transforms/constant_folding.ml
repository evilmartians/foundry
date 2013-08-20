open Unicode.Std
open Ssa

(* See http://www.cs.rice.edu/~keith/512/2011/Lectures/L19-SCCP-1up.pdf *)

type sccp_value =
| Top
| Const  of Rt.value
| Bottom

let sccp_inspect value =
  match value with
  | Top     -> "TOP"
  | Bottom  -> "BOT"
  | Const k -> Rt.inspect_value k

let sccp_inspect_list values =
  String.concat ", " (List.map sccp_inspect values)

let sccp_equal a b =
  match a, b with
  | Top,     Top     -> true
  | Bottom,  Bottom  -> true
  | Const a, Const b -> Rt.equal a b
  | _,       _       -> false

let run_on_function funcn =
  let cfg_worklist  = Worklist.create () in
  let ssa_worklist  = Worklist.create () in
  let cfg_reachable = Nametbl.create 10  in

  (* Entry is always visited. *)
  Worklist.put cfg_worklist (func_entry funcn);

  (* Create a hashtable to keep track of the currently proven
     facts about names. *)
  let values = Nametbl.create 10 in

  (* If the name doesn't exist in the table, it's "not yet known",
     that is top. *)
  let lookup name =
    match name.opcode with
    | Ssa.Const k
    -> Const k
    | _
    -> (try  Nametbl.find values name
        with Not_found -> Top)
  in

  (* If all of operands are known to be constants, returns Some values,
     otherwise None. *)
  let lookup_operands operands =
    let values = List.map lookup operands in
    try
      Some (List.map (fun operand ->
          match operand with
          | Const k -> k
          | _ -> raise Exit)
        values)
    with Exit ->
      None
  in

  (* Arguments are "known to be varying", that is bottom. *)
  iter_args funcn ~f:(fun arg -> Nametbl.add values arg Bottom);

  while (Worklist.some cfg_worklist) || (Worklist.some ssa_worklist) do
    (* Schedule a basic block unless it is already marked as reachable,
       and therefore visited. *)
    let schedule_block blockn =
      if not (Nametbl.mem cfg_reachable blockn) then
        Worklist.put cfg_worklist blockn
    in

    (* Schedule all reachable uses of instr for evaluation. *)
    let append_reachable_uses instr =
      iter_uses instr ~f:(fun use ->
        if Nametbl.mem cfg_reachable (instr_parent use) then
          Worklist.put ssa_worklist use)
    in

    (* Symbolically evaluate instr. *)
    let evaluate instr =
      match instr.opcode with
      (* Handle phis.
          * If all values in phi are TOP, the phi is TOP.
          * If all values in phi are the same constant, the phi is
            that constant.
          * Otherwise, the phi is BOT (varying). *)
      | PhiInstr operands
      -> (let values = List.map lookup (List.map snd operands) in
          List.fold_left (fun acc operand ->
              match acc, operand with
              (* TOP ^ x = x *)
              | Top, value | value, Top
              -> value
              (* Ci ^ Cj = Ci  | Ci = Cj *)
              | Const a, Const b when Rt.equal a b
              -> operand
              (* Ci ^ Cj = BOT | Ci ≠ Cj *)
              | _, _
              -> Bottom)
            Top values)
      (* Handle branches. Schedule the target; if possible, reduce
         conditional branches to unconditional. *)
      | JumpInstr target
      -> schedule_block target; Top
      | JumpIfInstr (cond, if_true, if_false)
      -> (match lookup cond with
          | Top
          -> ()
          | Bottom
          -> (schedule_block if_true;
              schedule_block if_false)
          | Const k
          -> (let target = if k = Rt.Truth then if_true else if_false in
              set_opcode (JumpInstr target) instr;
              schedule_block target)); Top
      (* Handle primitives. Fold non-side-effectful primitives, possibly
         with primitive-specific knowledge. *)
      | PrimitiveInstr (name, operands)
      -> (if Primitive.has_side_effects name then
            Bottom
          else
            match lookup_operands operands with
            | Some values -> Const (Primitive.invoke name values)
            | None -> Bottom)
      (* All unknown instructions are treated as varying by default; this
         is always safe. *)
      | _
      -> Bottom
    in

    (* Evaluate all instructions in basic blocks of cfg_worklist. *)
    while Worklist.some cfg_worklist do
      let blockn = Worklist.take cfg_worklist in
      Nametbl.add cfg_reachable blockn ();

      iter_instrs blockn ~f:(fun instr ->
        let value = evaluate instr in
        Nametbl.replace values instr value;
        append_reachable_uses instr)
    done;

    (* Evaluate all instructions scheduled in ssa_worklist, and
       re-schedule instructions with potentially updated values. *)
    while Worklist.some ssa_worklist do
      let instr = Worklist.take ssa_worklist in
      let value = evaluate instr in
      if not (sccp_equal (lookup instr) value) then begin
        Nametbl.replace values instr value;
        append_reachable_uses instr;
      end
    done
  done;

  (* Replace known constant instructions with their values. *)
  Nametbl.iter (fun instr value ->
      match value with
      | Const value
      -> replace_instr instr (name_of_value value)
      | _
      -> ())
    values

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
