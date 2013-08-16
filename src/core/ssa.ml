open Unicode.Std
open ExtList

type ty = Rt.ty

type name = {
  mutable id     : string;
  mutable ty     : Rt.ty;
  mutable opcode : opcode;

  (* Internal fields *)
  mutable parent : name option;
  mutable uses   : name list;
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

let func_of_name name =
  match name with
  | { opcode = Function func }
  | { opcode = BasicBlock(_);
      parent = Some { opcode = Function func } }
  -> func
  | _
  -> assert false

let block_of_name name =
  match name with
  | { opcode = BasicBlock bb }
   -> bb
  | _
  -> assert false

let mangle_id parent_v id =
  let func = func_of_name parent_v in
    let naming = func.naming in
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
    parent = None;
    uses   = [];
  }

let set_name_id name id =
  name.id <- mangle_id name id

let create_func ?(id="") ?arg_names args_ty result_ty =
  let func  = {
    arguments    = [];
    basic_blocks = [];
    naming       = {
      next_id = 0;
      names   = [""];
    }
  } in
  let value = {
    id;
    ty     = Rt.FunctionTy (args_ty, result_ty);
    opcode = Function func;
    parent = None;
    uses   = [];
  } in
  begin
    let make_arg name ty = {
      id     = mangle_id value name;
      ty;
      opcode = Argument;
      parent = Some value;
      uses   = [];
    } in
    match arg_names with
    | Some names ->
      func.arguments <- List.map2 make_arg names args_ty
    | None ->
      func.arguments <- List.map (make_arg "") args_ty
  end;
  value

let func_entry func =
  let func = func_of_name func in
    List.hd func.basic_blocks

let create_block ?(id="") func =
  let block = {
    id     = mangle_id func id;
    ty     = Rt.BasicBlockTy;
    opcode = BasicBlock { instructions = [] };
    parent = Some func;
    uses   = [];
  } in
    let func = func_of_name func in
      func.basic_blocks <- func.basic_blocks @ [block];
      block

let remove_block blockn =
  let func = func_of_name blockn in
  let _    = block_of_name blockn in
    func.basic_blocks <- List.remove func.basic_blocks blockn

let successors block_name =
  let block = block_of_name block_name in
  match (List.last block.instructions).opcode with
  | JumpInstr   target        -> [target]
  | JumpIfInstr (_, ift, iff) -> [ift; iff]
  | ReturnInstr _             -> []
  | _ -> assert false

let predecessors block_name =
  List.filter_map (fun use ->
      match use.opcode with
      | JumpInstr _ | JumpIfInstr _
      -> (match use.parent with
          | Some parent -> Some parent
          | None        -> assert false)
      | _
      -> None)
    block_name.uses

let uses_by_instr instr =
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
  List.iter (fun use -> use.uses <- instr :: use.uses)
    (uses_by_instr instr)

let remove_uses instr =
  List.iter (fun use -> use.uses <- List.remove use.uses instr)
    (uses_by_instr instr)

let append_instr ?(id="") ~ty ~opcode blockn =
  let block = block_of_name blockn in
    let instr = {
      id     = mangle_id blockn id;
      ty;
      opcode;
      parent = Some blockn;
      uses   = [];
    } in
      block.instructions <- block.instructions @ [instr];
      add_uses instr;
      instr

let replace_instr ?ty ?opcode instr =
  Option.may (fun ty ->
    instr.ty <- ty) ty;
  Option.may (fun opcode ->
    remove_uses instr;
    instr.opcode <- opcode;
    add_uses instr) opcode

let remove_instr instrn =
  ()
