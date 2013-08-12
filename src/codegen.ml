open Unicode.Std
open ExtList
open Big_int

module SsaNameIdentity =
struct
  type t = Ssa.name

  let equal = (==)
  let hash  = Hashtbl.hash
end

module Nametbl = Hashtbl.Make(SsaNameIdentity)

let ctx = Llvm.global_context ()

let gen_debug llmod =
  (* Declare int printf(char*, ...) *)
  let llprintf_ty = Llvm.var_arg_function_type (Llvm.i32_type ctx)
                      [| Llvm.pointer_type (Llvm.i8_type ctx) |] in
  let llprintf    = Llvm.declare_function "printf" llprintf_ty llmod in

  (* Define __fy_debug primitive helper *)
  let lldebug_ty  = Llvm.function_type (Llvm.void_type ctx) [| Llvm.i32_type ctx |] in
  let lldebug     = Llvm.declare_function "__fy_debug" lldebug_ty llmod in
  let entry       = Llvm.append_block ctx "entry" lldebug in
  let builder     = Llvm.builder ctx in
    Llvm.position_at_end entry builder;
    let llformat = Llvm.build_global_stringptr "[DEBUG: 0x%08x]\n" "__fy_format" builder in
    ignore (Llvm.build_call llprintf [| llformat; (Llvm.params lldebug).(0) |] "" builder);
    ignore (Llvm.build_ret_void builder);
    lldebug

let lltype_of_ty ty =
  match ty with
  | Rt.NilTy -> Llvm.void_type ctx
  | _ -> failwith (u"lltype_of_ty " ^ Rt.inspect_type ty)

let llconst_of_value value =
  match value with
  | Rt.Unsigned (width, ivalue)
  | Rt.Signed (width, ivalue)
  -> (let llty = Llvm.integer_type ctx width in
        if is_int_big_int ivalue then
          (* Fast path. *)
          Llvm.const_int llty (int_of_big_int ivalue)
        else try
          (* Slow path. *)
          let sext =
            match value with
            | Rt.Unsigned _ -> false
            | Rt.Signed   _ -> true
            | _             -> assert false
          in
          Llvm.const_of_int64 llty (int64_of_big_int ivalue) sext
        with Failure _ ->
          (* Very slow path. *)
          Llvm.const_int_of_string llty ((string_of_big_int ivalue) :> latin1s) 10)

let gen_operand names name =
  try
    Nametbl.find names name
  with Not_found ->
    match name.Ssa.opcode with
    | Ssa.Const value ->
      let llvalue = llconst_of_value value in
      Nametbl.add names name llvalue;
      llvalue
    | _ -> failwith (u"gen_operand " ^ name.Ssa.id)

let gen_prim llmod builder names (prim : string) operands =
  let map = gen_operand names in
  match (prim :> latin1s), operands with
  | "debug", [operand]
  -> (match Llvm.lookup_function "__fy_debug" llmod with
      | Some lldebug -> Llvm.build_call lldebug [| map operand |] "" builder
      | None -> assert false)
  | "int_add", [lhs; rhs]
  -> Llvm.build_add (map lhs) (map rhs) "" builder
  | _, _
  -> assert false

let gen_instr llmod builder names instrn =
  let llvalue =
    match instrn.Ssa.opcode with
    | Ssa.Const _
    -> assert false
    | Ssa.JumpInsn name
    -> Llvm.build_br (Llvm.block_of_value (Nametbl.find names name)) builder
    | Ssa.ReturnInsn name
    -> (match name.Ssa.ty with
        | Rt.NilTy -> Llvm.build_ret_void builder
        | _        -> Llvm.build_ret (gen_operand names name) builder)
    | Ssa.PrimitiveInsn (prim, operands)
    -> gen_prim llmod builder names prim operands
  in
  Nametbl.add names instrn llvalue

let gen_proto llmod name func_ty =
  let args_ty, ret_ty = func_ty in
  let llargs_ty = Array.of_list (List.map lltype_of_ty args_ty)
  and llret_ty  = lltype_of_ty ret_ty in
  let llty      = Llvm.function_type llret_ty llargs_ty in
    match Llvm.lookup_function name llmod with
    | None -> Llvm.declare_function name llty llmod
    | Some f ->
      assert (Llvm.block_begin f == Llvm.At_end f);
      assert (Llvm.element_type (Llvm.type_of f) == llty);
      f

let gen_func llmod funcn =
  let func = Ssa.func_of_name funcn in
  let ty =
    match funcn.Ssa.ty with
    | Rt.FunctionTy (args, ret) -> args, ret
    | _ -> assert false
  in
  let llfunc = gen_proto llmod (funcn.Ssa.id :> latin1s) ty in
  let builder = Llvm.builder ctx in
    (* Define a map between FSSA names and LLVM names *)
    let names = Nametbl.create 10 in

    (* Rename the LLVM arguments to match FSSA arguments,
       and add them to the value map. *)
    let llparams = Llvm.params llfunc in
    List.iteri (fun index argn ->
        let llparam = llparams.(index) in
        Nametbl.add names argn llparam;
        Llvm.set_value_name (argn.Ssa.id :> latin1s) llparam)
      func.Ssa.arguments;

    (* Create LLVM basic blocks for corresponding FSSA basic
       blocks. *)
    List.iter (fun blockn ->
        let llblock = Llvm.append_block ctx (blockn.Ssa.id :> latin1s) llfunc in
        Nametbl.add names blockn (Llvm.value_of_block llblock))
      func.Ssa.basic_blocks;

    (* Codegen non-phi instructions while traversing blocks in
       domination order. *)
    List.iter (fun blockn ->
        let block   = Ssa.basic_block_of_name blockn in
        let llblock = Llvm.block_of_value (Nametbl.find names blockn) in
        Llvm.position_at_end llblock builder;
        List.iter (gen_instr llmod builder names) block.Ssa.instructions)
      func.Ssa.basic_blocks;

    (* Fixup phi instructions. *)

    (* Validate the result. *)
    if not (Llvm_analysis.verify_function llfunc) then begin
      Llvm.dump_value llfunc;
      Llvm_analysis.assert_valid_function llfunc
    end;

    llfunc

let llvm_module_of_ssa_func name =
  let llmod = Llvm.create_module ctx "foundry" in
    ignore (gen_debug llmod);
    ignore (gen_func llmod name);
    llmod
