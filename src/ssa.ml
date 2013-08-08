open Unicode.Std

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
| Function   of func
| Argument
| BasicBlock of basic_block
(* Phi *)
| Phi        of (name * name) list
(* Terminators *)
| Jump       of name
| JumpIf     of name * name * name
| Return     of name
(* Language-specific opcodes *)
| Const      of Rt.value
| Primitive0 of string
| Primitive1 of string * name
| Primitive2 of string * name * name

let mangle_id parent_v id =
  match parent_v with
  | { opcode = Function func }
  | { opcode = BasicBlock(_);
      parent = Some { opcode = Function func } }
  -> (let naming = func.naming in
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
        end)
  | _ -> assert false

let create_func ?(id="") args_ty ret_ty =
  let func  = {
    arguments = [];
    naming    = {
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

let func_of_lambda ?(id="") lambda =
  let ty   = lambda.Rt.l_ty in
    let func = create_func ~id
                  [ty.Rt.l_args_ty; ty.Rt.l_kwargs_ty]
                  ty.Rt.l_result_ty in
      func
