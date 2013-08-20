open Unicode.Std
open Ssa

(* See http://www.cs.rice.edu/~keith/512/2011/Lectures/L19-SCCP-1up.pdf *)

type sccp_value =
| Top
| Const  of Rt.value
| Bottom

let equal a b =
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
    let append_reachable_uses instr =
      iter_uses instr ~f:(fun use ->
        if Nametbl.mem cfg_reachable (instr_parent use) then
          Worklist.put ssa_worklist use)
    in

    let evaluate instr =
      match instr.opcode with
      | PrimitiveInstr (name, operands)
      -> (if Primitive.has_side_effects name then
            Bottom
          else
            match lookup_operands operands with
            | Some values -> Const (Primitive.invoke name values)
            | None -> Bottom)
      | _
      -> Bottom
    in

    while Worklist.some cfg_worklist do
      let blockn = Worklist.take cfg_worklist in
      Nametbl.add cfg_reachable blockn ();

      iter_instrs blockn ~f:(fun instr ->
        let value = evaluate instr in
        Nametbl.replace values instr value;
        append_reachable_uses instr)
    done;

    while Worklist.some ssa_worklist do
      let instr = Worklist.take ssa_worklist in
      let value = evaluate instr in
      if not (equal (lookup instr) value) then begin
        Nametbl.replace values instr value;
        append_reachable_uses instr;
      end
    done
  done;

  Nametbl.iter (fun instr value ->
      match value with
      | Const value
      -> replace_instr instr (name_of_value value)
      | _
      -> ())
    values

let run_on_capsule capsule =
  List.iter run_on_function capsule.functions
