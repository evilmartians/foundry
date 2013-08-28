open Unicode.Std
open Big_int
open Ssa

let name = "Constant Folding"

(* See http://www.cs.rice.edu/~keith/512/2011/Lectures/L19-SCCP-1up.pdf *)

exception Varying

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

let run_on_function passmgr capsule funcn =
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

  (* Either returns a constant value (values), or raises Varying, which
     marks the current instruction as Bottom. *)
  let get name =
    match lookup name with
    | Const k -> k
    | _ -> raise Varying
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
    let evaluate' instr =
      match instr.opcode with
      | Ssa.Const k
      -> Const k

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
              set_opcode instr (JumpInstr target);
              schedule_block target)); Top

      (* Tuple instructions. *)
      | TupleExtendInstr (tup, elems)
      -> (let tup =
            match get tup with
            | Rt.Tuple xs -> xs
            | _ -> assert false
          and elems = List.map get elems in
          Const (Rt.Tuple (tup @ elems)))
      | TupleConcatInstr (tup, tup')
      -> (match get tup, get tup' with
          | Rt.Tuple xs, Rt.Tuple ys
          -> Const (Rt.Tuple (xs @ ys))
          | _
          -> assert false)
      | TupleConcatInstr ({ opcode = Ssa.Const (Rt.Tuple []) }, tup)
      | TupleConcatInstr (tup, { opcode = Ssa.Const (Rt.Tuple []) })
      -> lookup tup

      (* Record instructions. *)
      | RecordExtendInstr (tup, elems)
      -> (let tup =
            match get tup with
            | Rt.Record xs -> xs
            | _ -> assert false
          and elems =
            List.map (fun (key, value) ->
                match get key with
                | Rt.Symbol key -> key, get value
                | _ -> assert false)
              elems
          in
          let elems = Assoc.sorted elems in
          Const (Rt.Record (Assoc.merge tup elems)))
      | RecordConcatInstr (tup, tup')
      -> (match get tup, get tup' with
          | Rt.Record xs, Rt.Record ys
          -> Const (Rt.Record (Assoc.merge xs ys))
          | _
          -> assert false)
      | RecordConcatInstr ({ opcode = Ssa.Const (Rt.Record xs) }, re)
      | RecordConcatInstr (re, { opcode = Ssa.Const (Rt.Record xs) })
      -> (if Assoc.is_empty xs then lookup re
          else Bottom)

      (* Handle specialization. *)
      | SpecializeInstr (cls, specz')
      -> (let specz' = Assoc.map specz' ~f:(fun _ -> get) in
          Const (Typing.equiv (get cls) ~f:(fun (klass, specz) ->
            Rt.Class (klass, Assoc.merge specz specz'))))

      (* Handle primitives. Fold non-side-effectful primitives, possibly
         with primitive-specific knowledge. *)
      | PrimitiveInstr (name, operands)
      -> (match (name :> latin1s), operands with
          | "tup_length", [tup]
          -> (match tup.ty with
              | Rt.TupleTy(xs) -> Const (Rt.Integer (big_int_of_int (List.length xs)))
              | _ -> Bottom)
          | _
          -> (if Primitive.has_side_effects name then
                Bottom
              else
                Const (Primitive.invoke name (List.map get operands))))

      (* All unknown instructions are treated as varying by default; this
         is always safe. *)
      | _
      -> Bottom
    in

    let evaluate instr =
      try  evaluate' instr
      with Varying -> Bottom
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
      -> (* Constant propagation has likely opened more transformation
            opportunities. *)
         (Pass_manager.mark ~reason:("%" ^ instr.id ^ " = " ^
                                (Rt.inspect_value value)) passmgr funcn;
          replace_instr instr (const value))
      | _
      -> ())
    values;
