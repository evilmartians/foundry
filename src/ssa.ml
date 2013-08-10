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
(* Functions *)
| Function        of func
| Argument
| BasicBlock      of basic_block
(* Constants *)
| Const           of Rt.value
(* Phi *)
| PhiInsn         of (name * name) list
(* Terminators *)
| JumpInsn        of name
| JumpIfInsn      of name * name * name
| ReturnInsn      of name
(* Language-specific opcodes *)
| EnvironmentInsn of Rt.bindings_ty * (*parent*) name
| LVarLoadInsn    of name * string
| LVarStoreInsn   of name * string * name
| Primitive0Insn  of string
| Primitive1Insn  of string * name
| Primitive2Insn  of string * name * name

let func_of_name name =
  match name with
  | { opcode = Function func }
  | { opcode = BasicBlock(_);
      parent = Some { opcode = Function func } }
  -> func
  | _
  -> assert false

let basic_block_of_name name =
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

let create_func ?(id="") args_ty ret_ty =
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
      ty     = Rt.FunctionTy (args_ty, ret_ty);
      opcode = Function func;
      parent = None;
      uses   = [];
    } in
      let make_arg ty = {
        id     = mangle_id value "";
        ty;
        opcode = Argument;
        parent = Some value;
        uses   = [];
      } in
        func.arguments <- List.map make_arg args_ty;
        value

let create_basic_block ?(id="") ~parent =
  {
    id     = mangle_id parent id;
    ty     = Rt.BasicBlockTy;
    opcode = BasicBlock { instructions = [] };
    parent = Some parent;
    uses   = [];
  }

let entry func =
  let func = func_of_name func in
    List.hd func.basic_blocks

let add_basic_block func basic_block_v =
  let func = func_of_name func in
  let _    = basic_block_of_name basic_block_v in
    func.basic_blocks <- func.basic_blocks @ [basic_block_v]

let remove_basic_block basic_block_v =
  let func = func_of_name basic_block_v in
  let _    = basic_block_of_name basic_block_v in
    func.basic_blocks <- List.remove func.basic_blocks basic_block_v

let const value =
  {
    id     = "";
    ty     = Rt.type_of_value value;
    opcode = Const value;
    parent = None;
    uses   = [];
  }

let append_insn basic_block_v ?(id="") ty opcode =
  let basic_block = basic_block_of_name basic_block_v in
    let insn = {
      id     = mangle_id basic_block_v id;
      ty;
      opcode;
      parent = Some basic_block_v;
      uses   = [];
    } in
      basic_block.instructions <- basic_block.instructions @ [insn];
      insn

let replace_insn insn_v ?id ty opcode =
  ()

let remove_insn insn_v =
  ()

type ssa_conv_state = {
  lambda      : Rt.lambda;
  current_env : name;
}

let lvar_type ty name =
  let rec lookup env =
    match Table.get env.Rt.e_bindings_ty name with
    | Some binding -> binding.Rt.b_value_ty
    | None
    -> (match env.Rt.e_parent_ty with
        | Some parent_ty -> lookup parent_ty
        | None -> assert false)
  in
  match ty with
  | Rt.EnvironmentTy env -> lookup env
  | _ -> assert false

let ssa_of_formal_args ~entry =
  ()

let rec ssa_of_expr ~entry ~state ~expr =
  match expr with
  | Syntax.Begin (_, exprs)
  -> ssa_of_exprs ~entry ~state ~exprs
  | Syntax.Int (_, n)
  -> entry, const (Rt.Integer n)
  | Syntax.Var (_, name)
  -> entry, append_insn entry
                (lvar_type state.current_env.ty name)
                (LVarLoadInsn (state.current_env, name))
  | _
  -> failwith ("ssa_of_expr: " ^
        (Unicode.assert_utf8s
          (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr))))

and ssa_of_exprs ~entry ~state ~exprs =
  (List.fold_left
    (fun (entry, name) expr -> ssa_of_expr ~entry ~state ~expr)
    (entry, const Rt.Nil)
    exprs)

let name_of_lambda ?(id="") lambda =
  let func =
    let ty = lambda.Rt.l_ty in
      create_func ~id
        [ty.Rt.l_args_ty; ty.Rt.l_kwargs_ty]
        ty.Rt.l_result_ty
  in
  let entry = create_basic_block ~id:"entry" ~parent:func in
    add_basic_block func entry;
    let parent_env =
      (Rt.Environment lambda.Rt.l_local_env)
    in
    let current_env =
      append_insn entry
        (Rt.type_of_value parent_env)
        (EnvironmentInsn (Table.create [], const parent_env))
    in
    let state = { lambda; current_env; }
    in
    let entry, name = ssa_of_exprs ~entry ~state ~exprs:lambda.Rt.l_body in
      append_insn entry Rt.NilTy (ReturnInsn name);
      func
