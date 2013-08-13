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
  (* Declare int printf(char*, ...). *)
  let llprintf_ty = Llvm.var_arg_function_type (Llvm.i32_type ctx)
                      [| Llvm.pointer_type (Llvm.i8_type ctx) |] in
  let llprintf    = Llvm.declare_function "printf" llprintf_ty llmod in

  (* Define __fy_debug primitive helper. *)
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
          (* Very slow path. There is no direct big_int -> llvalue converter. *)
          Llvm.const_int_of_string llty ((string_of_big_int ivalue) :> latin1s) 10)
  | _ -> assert false

let gen_proto llmod name func_ty =
  let args_ty, ret_ty = func_ty in
  (* Map FSSA prototype to LLVM prototype. *)
  let llargs_ty = Array.of_list (List.map lltype_of_ty args_ty)
  and llret_ty  = lltype_of_ty ret_ty in
  let llty      = Llvm.function_type llret_ty llargs_ty in
    (* Lookup or create a function with the specified name and
       verify that its prototype matches. *)
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
  let llfunc  = gen_proto llmod (funcn.Ssa.id :> latin1s) ty in
  let builder = Llvm.builder ctx in

  (* Define a map between FSSA names and LLVM names *)
  let names   = Nametbl.create 10 in

  (* Define list of LLVM phis which need to be fixed up
     and FSSA names of incoming values. *)
  let fixups  = ref [] in

  (* Map FSSA name to LLVM name, and memoize the mapping. *)
  let map name =
    try
      Nametbl.find names name
    with Not_found ->
      match name.Ssa.opcode with
      | Ssa.Const value ->
        let llvalue = llconst_of_value value in
        Nametbl.add names name llvalue;
        llvalue
      | _ -> failwith (u"gen_func/map " ^ name.Ssa.id)
  in

  (* Map FSSA primitive to LLVM primitive. *)
  let gen_prim id prim operands =
    let module Icmp = Llvm.Icmp in
    match prim, operands with
    | "debug", [operand]
    -> (match Llvm.lookup_function "__fy_debug" llmod with
        | Some lldebug -> Llvm.build_call lldebug [| map operand |] "" builder
        | None -> assert false)
    | "int_add",  [lhs; rhs] -> Llvm.build_add  (map lhs) (map rhs) id builder
    | "int_sub",  [lhs; rhs] -> Llvm.build_sub  (map lhs) (map rhs) id builder
    | "int_mul",  [lhs; rhs] -> Llvm.build_mul  (map lhs) (map rhs) id builder
    | "int_udiv", [lhs; rhs] -> Llvm.build_udiv (map lhs) (map rhs) id builder
    | "int_sdiv", [lhs; rhs] -> Llvm.build_sdiv (map lhs) (map rhs) id builder
    | "int_and",  [lhs; rhs] -> Llvm.build_and  (map lhs) (map rhs) id builder
    | "int_or",   [lhs; rhs] -> Llvm.build_or   (map lhs) (map rhs) id builder
    | "int_xor",  [lhs; rhs] -> Llvm.build_xor  (map lhs) (map rhs) id builder
    | "int_shl",  [lhs; rhs] -> Llvm.build_shl  (map lhs) (map rhs) id builder
    | "int_lshr", [lhs; rhs] -> Llvm.build_lshr (map lhs) (map rhs) id builder
    | "int_ashr", [lhs; rhs] -> Llvm.build_ashr (map lhs) (map rhs) id builder
    | "int_eq",   [lhs; rhs] -> Llvm.build_icmp Icmp.Eq  (map lhs) (map rhs) id builder
    | "int_neq",  [lhs; rhs] -> Llvm.build_icmp Icmp.Ne  (map lhs) (map rhs) id builder
    | "int_ule",  [lhs; rhs] -> Llvm.build_icmp Icmp.Ule (map lhs) (map rhs) id builder
    | "int_ult",  [lhs; rhs] -> Llvm.build_icmp Icmp.Ult (map lhs) (map rhs) id builder
    | "int_uge",  [lhs; rhs] -> Llvm.build_icmp Icmp.Uge (map lhs) (map rhs) id builder
    | "int_ugt",  [lhs; rhs] -> Llvm.build_icmp Icmp.Ugt (map lhs) (map rhs) id builder
    | "int_sle",  [lhs; rhs] -> Llvm.build_icmp Icmp.Sle (map lhs) (map rhs) id builder
    | "int_slt",  [lhs; rhs] -> Llvm.build_icmp Icmp.Slt (map lhs) (map rhs) id builder
    | "int_sge",  [lhs; rhs] -> Llvm.build_icmp Icmp.Sge (map lhs) (map rhs) id builder
    | "int_sgt",  [lhs; rhs] -> Llvm.build_icmp Icmp.Sgt (map lhs) (map rhs) id builder
    | _, _
    -> failwith (u"gen_func/gen_prim: " ^ (Unicode.assert_utf8s prim) ^
                 u"/" ^ (string_of_int (List.length operands)))
  in

  (* Build LLVM instruction out of FSSA instruction. *)
  let gen_instr instrn =
    let id = (instrn.Ssa.id :> latin1s) in
    let llvalue =
      match instrn.Ssa.opcode with
      (* Constants are handled within `map'. *)
      | Ssa.Const _
      -> assert false
      (* Terminator instructions. *)
      | Ssa.JumpInsn blockn
      -> Llvm.build_br (Llvm.block_of_value (Nametbl.find names blockn)) builder
      | Ssa.JumpIfInsn (condn, truen, falsen)
      -> Llvm.build_cond_br (map condn)
              (Llvm.block_of_value (map truen))
              (Llvm.block_of_value (map falsen))
            builder
      | Ssa.ReturnInsn valuen
      -> (match valuen.Ssa.ty with
          | Rt.NilTy -> Llvm.build_ret_void builder
          | _        -> Llvm.build_ret (map valuen) builder)
      (* Value-returning instructions. *)
      | Ssa.PhiInsn operands
      -> (* Divide all FSSA incoming values into predecessor values
            and successor values. It is assumed here that the basic blocks
            are sorted in domination order, therefore all the names which
            are already converted to LLVM come from dominating blocks. *)
         (let pred, succ = List.partition (fun (_, op) ->
                              match op.Ssa.opcode with
                              | Ssa.Const _ -> true
                              | _ -> Nametbl.mem names op) operands
          in
          (* Build an LLVM phi with existing values. Attempting to pass []
             to build_phi results in segfault :( *)
          let incoming = List.map (fun (block, op) ->
                            map op, Llvm.block_of_value (map block)) pred in
          let llphi    = Llvm.build_phi incoming id builder in
          (* Add all not yet existing values into the fixup list. *)
          List.iter (fun (block, op) -> fixups := (llphi, block, op) :: !fixups) succ;
          llphi)
      | Ssa.PrimitiveInsn (prim, operands)
      -> gen_prim id (prim :> latin1s) operands
      | _
      -> assert false
    in
    Nametbl.add names instrn llvalue
  in

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
      List.iter gen_instr block.Ssa.instructions)
    func.Ssa.basic_blocks;

  (* Fixup phi instructions. *)
  List.iter (fun (llphi, pred, name) ->
      let llblock = Llvm.block_of_value (Nametbl.find names pred) in
      let llvalue = map name in
      Llvm.add_incoming (llvalue, llblock) llphi)
    !fixups;

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
