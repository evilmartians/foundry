open Unicode.Std
open ExtList

type ty = Rt.ty

type name = {
  mutable id     : string;
  mutable ty     : Rt.ty;
  mutable opcode : opcode;

  (* Internal fields *)
  mutable parent : name_parent;
  mutable uses   : name list;
}
and name_parent =
| ParentNone
| ParentCapsule    of capsule
| ParentFunction   of name
| ParentBasicBlock of name
and capsule = {
  mutable functions    : name list;
}
and func = {
          naming       : func_naming;
  mutable arguments    : name list;
  mutable basic_blocks : name list;
}
and func_naming = {
  mutable next_id      : int;
  mutable names        : string list;
}
and basic_block = {
  mutable instructions : name list;
}
and opcode =
| InvalidInstr
(* Functions *)
| Function        of func
| Argument
| BasicBlock      of basic_block
(* Constants *)
| Const           of Rt.value
(* Phi *)
| PhiInstr        of ((*basic_block*) name * (*value*) name) list
(* Terminators *)
| JumpInstr       of (*target*) name
| JumpIfInstr     of (*condition*) name * (*if_true*) name * (*if_false*) name
| ReturnInstr     of (*value*) name
(* Language-specific opcodes *)
| FrameInstr      of (*parent*) name
| LVarLoadInstr   of (*environment*) name * (*var*) string
| LVarStoreInstr  of (*environment*) name * (*var*) string * (*value*) name
| CallInstr       of (*func*) name   * (*operands*) name list
| PrimitiveInstr  of (*name*) string * (*operands*) name list

(* Variable naming convention:
   funcn:  function name, of type name
   func:   function, of type func
   blockn: block name, of type name
   block:  block, of type basic_block
   instr:  instruction name, of type name
 *)

let func_of_name name =
  match name with
  | { opcode = Function func }
  -> func
  | _
  -> assert false

let block_of_name name =
  match name with
  | { opcode = BasicBlock block }
   -> block
  | _
  -> assert false

let mangle_id name id =
  let naming =
    match name with
    | { opcode = Function func }
    | { parent = ParentFunction { opcode = Function func } }
    -> func.naming
    | _
    -> assert false
  in
  if id = "" then begin
    naming.next_id <- naming.next_id + 1;
      (string_of_int naming.next_id)
  end else begin
    let id =
      if List.exists ((=) id) naming.names then begin
        naming.next_id <- naming.next_id + 1;
        id ^ "." ^ (string_of_int naming.next_id)
      end else id
    in begin
      naming.names <- id :: naming.names;
      id
    end
  end

let name_of_value value =
  {
    id     = "";
    ty     = Rt.type_of_value value;
    opcode = Const value;
    parent = ParentNone;
    uses   = [];
  }

let set_id id name =
  name.id <- mangle_id name id

let create_capsule () =
  { functions = []; }

let iter_funcs ~f capsule =
  List.iter f capsule.functions

let find_func id capsule =
  List.find (fun funcn -> funcn.id = id) capsule.functions

let add_func funcn capsule =
  capsule.functions <- funcn :: capsule.functions

let remove_func funcn capsule =
  capsule.functions <- List.remove capsule.functions funcn

let create_func ?(id="") ?arg_ids args_ty result_ty =
  let func  = {
    arguments    = [];
    basic_blocks = [];
    naming       = {
      next_id = 0;
      names   = [""];
    }
  } in
  let funcn = {
    id;
    ty     = Rt.FunctionTy (args_ty, result_ty);
    opcode = Function func;
    parent = ParentNone;
    uses   = [];
  } in
  begin
    let make_arg name ty = {
      id     = mangle_id funcn name;
      ty;
      opcode = Argument;
      parent = ParentFunction funcn;
      uses   = [];
    } in
    match arg_ids with
    | Some ids ->
      func.arguments <- List.map2 make_arg ids args_ty
    | None ->
      func.arguments <- List.map (make_arg "") args_ty
  end;
  funcn

let func_ty funcn =
  match funcn.ty with
  | Rt.FunctionTy (args_ty, ret_ty) -> args_ty, ret_ty
  | _ -> assert false

let func_entry funcn =
  let func = func_of_name funcn in
  List.hd func.basic_blocks

let iter_blocks ~f funcn =
  let func = func_of_name funcn in
  List.iter f func.basic_blocks

let create_block ?(id="") funcn =
  let func = func_of_name funcn in
  let block = {
    id     = mangle_id funcn id;
    ty     = Rt.BasicBlockTy;
    opcode = BasicBlock { instructions = [] };
    parent = ParentFunction funcn;
    uses   = [];
  } in
  func.basic_blocks <- func.basic_blocks @ [block];
  block

let remove_block blockn =
  match blockn.parent with
  | ParentFunction funcn
  -> (let func = func_of_name funcn in
      func.basic_blocks <- List.remove_if ((==) blockn) func.basic_blocks)
  | _
  -> assert false

let iter_instrs ~f name =
  match name with
  | { opcode = Function func }
  -> (List.iter (fun blockn ->
          let block = block_of_name blockn in
          List.iter f block.instructions)
        func.basic_blocks)
  | { opcode = BasicBlock block }
  -> List.iter f block.instructions
  | _
  -> assert false

let terminator blockn =
  let block = block_of_name blockn in
  (List.last block.instructions)

let successors blockn =
  match (terminator blockn).opcode with
  | JumpInstr   target        -> [target]
  | JumpIfInstr (_, ift, iff) -> [ift; iff]
  | ReturnInstr _             -> []
  | _ -> assert false

let predecessors blockn =
  List.filter_map (fun use ->
      match use.opcode with
      | JumpInstr _ | JumpIfInstr _
      -> (match use.parent with
          | ParentBasicBlock block -> Some block
          | _ -> assert false)
      | _
      -> None)
    blockn.uses

let instr_operands instr =
  match instr.opcode with
  | InvalidInstr
  | Argument
  | Function _
  | BasicBlock _
  | Const _
  -> []
  | PhiInstr operands
  -> (List.map fst operands) @ (List.map snd operands)
  | JumpInstr target
  -> [target]
  | JumpIfInstr (cond, if_true, if_false)
  -> [cond; if_true; if_false]
  | ReturnInstr value
  -> [value]
  | FrameInstr parent
  -> [parent]
  | LVarLoadInstr (env, _)
  -> [env]
  | LVarStoreInstr (env, _, value)
  -> [env; value]
  | CallInstr (func, operands)
  -> func :: operands
  | PrimitiveInstr (name, operands)
  -> operands

let add_uses instr =
  List.iter (fun used ->
      assert (not (List.memq instr used.uses));
      used.uses <- instr :: used.uses)
    (instr_operands instr)

let remove_uses instr =
  List.iter (fun used ->
      assert (List.memq instr used.uses);
      used.uses <- List.remove_if ((==) instr) used.uses)
    (instr_operands instr)

let iter_uses ~f instr =
  List.iter f instr.uses

let create_instr ?(id="") ty opcode =
  {
    id;
    ty;
    opcode;
    parent = ParentNone;
    uses   = [];
  }

let insert_instr ?pivot f_some f_none instr blockn =
  let block = block_of_name blockn in
  begin
    (* Sanity checks: instr must be an instruction not
       attached to any block. *)
    assert (instr.parent = ParentNone);
    match instr.opcode with
    | Argument | Function _ | BasicBlock _
    | Const _
    -> assert false;
    | _
    -> ()
  end;
  begin
    match pivot with
    | Some pivotn
    -> (assert (List.memq pivotn block.instructions);
        block.instructions <-
          List.fold_left (fun new_instrs curn ->
              if curn == pivotn then
                f_some new_instrs curn
              else curn :: new_instrs)
            [] block.instructions)
    | None
    -> block.instructions <- f_none block.instructions
  end;
  instr.id     <- mangle_id blockn instr.id;
  instr.parent <- ParentBasicBlock blockn

let prepend_instr ?before instr blockn =
  insert_instr ?pivot:before
      (fun instrs curn -> instr :: curn :: instrs)
      (fun instrs      -> instr :: instrs)
    instr blockn

let append_instr ?after instr blockn =
  insert_instr ?pivot:after
      (fun instrs curn -> curn :: instr :: instrs)
      (fun instrs      -> instrs @ [instr])
    instr blockn

let set_opcode opcode name =
  match name.opcode with
  | Argument | Function _ | BasicBlock _ | Const _
  -> assert false
  | _ (* instruction *)
  -> (remove_uses name;
      name.opcode <- opcode;
      add_uses name)

let set_ty ty name =
  match name.opcode with
  | Function func
  -> (match ty with
      | Rt.FunctionTy (args_ty, ret_ty)
      -> (name.ty <- ty;
          List.iter2 (fun arg ty -> arg.ty <- ty)
              func.arguments args_ty)
      | _
      -> assert false)
  | Argument | BasicBlock _ | Const _
  -> assert false
  | _ (* instruction *)
  -> name.ty <- ty

let remove_instr instr =
  match instr.parent with
  | ParentBasicBlock blockn
  -> (let block = block_of_name blockn in
      assert (List.memq instr block.instructions);
      block.instructions <- List.remove_if ((==) instr) block.instructions;
      instr.parent <- ParentNone)
  | _
  -> assert false

let erase_instr instr =
  remove_instr instr;
  remove_uses instr;
  instr.opcode <- InvalidInstr

let map_instr_operands instr operands =
  match instr.opcode, operands with
  | InvalidInstr, []
  | Argument,     []
  | Function _,   []
  | BasicBlock _, []
  | Const _,      []
  -> instr.opcode
  | PhiInstr _, _
  -> (let halfway = (List.length operands) / 2 in
      let blocks, values = List.split_nth halfway operands in
      let operands' = List.combine blocks values in
      PhiInstr operands')
  | JumpInstr _,   [target]
  -> JumpInstr target
  | JumpIfInstr _, [cond; if_true; if_false]
  -> JumpIfInstr (cond, if_true, if_false)
  | ReturnInstr _, [value]
  -> ReturnInstr value
  | FrameInstr _,  [parent]
  -> FrameInstr parent
  | LVarLoadInstr (_, name), [frame]
  -> LVarLoadInstr (frame, name)
  | LVarStoreInstr (_, name, _), [frame; value]
  -> LVarStoreInstr (frame, name, value)
  | CallInstr (func, _), _
  -> CallInstr (func, operands)
  | PrimitiveInstr (name, _), _
  -> PrimitiveInstr (name, operands)
  | _
  -> assert false

let set_instr_operands instr operands =
  set_opcode (map_instr_operands instr operands) instr

let replace_all_uses_with instr instr' =
  iter_uses instr ~f:(fun use ->
    let operands = instr_operands use in
    let operands = List.map (fun operand ->
      if operand == instr then instr'
      else operand) operands
    in
    set_instr_operands use operands)

let replace_instr instr instr' =
  replace_all_uses_with instr instr';
  match instr' with
  | { opcode = Argument     }
  | { opcode = Function _   }
  | { opcode = BasicBlock _ }
  | { opcode = Const _      }
  -> erase_instr instr;
  | { parent = ParentBasicBlock block }
  -> prepend_instr ~before:instr instr' block;
     erase_instr instr
  | _
  -> assert false

let copy_func funcn =
  let func    = func_of_name funcn in
  (* Duplicate the function. *)
  let arg_ids = List.map (fun arg -> arg.id) func.arguments in
  let args_ty, ret_ty = func_ty funcn in
  let funcn' = create_func ~id:funcn.id ~arg_ids args_ty ret_ty in
  let func'  = func_of_name funcn' in
  (* Duplicate function content while maintaining referentional
     integrity. *)
  let map    = Hashtbl.create 10 in
  (* Remember the mapping between arguments. *)
  List.iter2 (Hashtbl.add map) func.arguments func'.arguments;
  (* Duplicate basic blocks and remember the mapping between them. *)
  iter_blocks funcn ~f:(fun blockn ->
    let blockn' = create_block ~id:blockn.id funcn' in
    Hashtbl.add map blockn blockn');

  (* Duplicate instructions while updating references through
     the mapping and remember the mapping. *)
  let map_opcode instr instr' =
    let map_operand operand =
      match operand.opcode with
      (* Don't map calls to specialized function in its own body to
         this specialization: the function may call itself with
         a different signature. *)
      | Function _
      (* Constants are module-global. *)
      | Const _
      -> operand
      | _
      -> Hashtbl.find map operand
    in
    let operands' = List.map map_operand (instr_operands instr) in
    let opcode'   = map_instr_operands instr operands' in
    set_opcode opcode' instr'
  in
  let phis = ref [] in
  iter_instrs funcn ~f:(fun instr ->
    (* Duplicate instruction. *)
    let instr'  = create_instr ~id:instr.id instr.ty InvalidInstr in
    (* Append instruction to the corresponding basic block in the
       specialized function. *)
    let blockn' =
      match instr.parent with
      | ParentBasicBlock blockn -> Hashtbl.find map blockn
      | _ -> assert false
    in
    append_instr instr' blockn';
    (* Remember the mapping for the current instruction. *)
    Hashtbl.add map instr instr';
    (* Map instruction operands. *)
    match instr.opcode with
    (* Phi instructions will be fixed up later. *)
    | PhiInstr _ -> phis := (instr, instr') :: !phis
    | _          -> map_opcode instr instr');
  (* Fix up all phis. *)
  List.iter (fun (phi, phi') -> map_opcode phi phi') !phis;
  funcn'

let specialize funcn env =
  let rewrite = Typing.rewrite env in
  set_ty (rewrite funcn.ty) funcn;
  iter_instrs funcn ~f:(fun instr ->
    set_ty (rewrite instr.ty) instr)
