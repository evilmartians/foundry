open ExtList
open Fy_big_int

let ctx = Llvm.global_context ()

let gen_debug llmod =
  (* Declare int printf(char*, ...). *)
  let llprintf_ty = Llvm.var_arg_function_type (Llvm.i32_type ctx)
                      [| Llvm.pointer_type (Llvm.i8_type ctx) |] in
  let llprintf    = Llvm.declare_function "printf" llprintf_ty llmod in

  (* Define __fy_debug primitive helper. *)
  let lldebug_ty  = Llvm.function_type (Llvm.void_type ctx) [| Llvm.i32_type ctx |] in
  let lldebug     = Llvm.declare_function "__fy_debug" lldebug_ty llmod in
  let entry       = Llvm.append_block ctx "" lldebug in
  let builder     = Llvm.builder ctx in
    Llvm.position_at_end entry builder;
    let llformat = Llvm.build_global_stringptr "[DEBUG: 0x%08x]\n" "__fy_format" builder in
    ignore (Llvm.build_call llprintf [| llformat; (Llvm.params lldebug).(0) |] "" builder);
    ignore (Llvm.build_ret_void builder);
    lldebug

let types = Rt.Valuetbl.create 10

let rec lltype_of_ty ?(ptr=true) ty =
  let memoize name generate =
    let llty =
      try
        Rt.Valuetbl.find types ty
      with Not_found ->
        (* Create and populate a named struct type. *)
        let llty = Llvm.named_struct_type ctx name in
        let is_packed, elts = generate () in
        Llvm.struct_set_body llty (Array.of_list elts) is_packed;
        (* Memoize and return this type. *)
        Rt.Valuetbl.add types ty llty;
        llty
    in
    if ptr then Llvm.pointer_type llty else llty
  in
  match ty with
  | Rt.NilTy
  -> Llvm.struct_type ctx [||]
  | Rt.BooleanTy
  -> Llvm.i1_type ctx
  | Rt.UnsignedTy width
  | Rt.SignedTy width
  -> Llvm.integer_type ctx width
  | Rt.TupleTy xs
  -> Llvm.struct_type ctx (Array.of_list (List.map lltype_of_ty xs))
  | Rt.RecordTy xs
  -> Llvm.struct_type ctx (Array.of_list
        (Table.map_list ~ordered:true ~f:(fun _ -> lltype_of_ty ~ptr:false) xs))
  | Rt.EnvironmentTy env_ty
  -> Llvm.pointer_type (Llvm.i8_type ctx)
  | Rt.Class (klass, specz)
  -> (memoize (klass.Rt.k_name :> string) (fun () ->
        let slots = List.map (fun (name, _) ->
                        lltype_of_ty (Typing.slot_ty (klass, specz) name))
                      klass.Rt.k_slots in
        let slots =
          match klass.Rt.k_ancestor with
          | None -> slots
          | Some ancestor -> (lltype_of_ty ~ptr:false (Rt.Class (ancestor, specz))) :: slots
        in
        false, slots))
  | Rt.FunctionTy (args_ty, ret_ty)
  -> (Llvm.function_type
        (lltype_of_ty ret_ty)
        (Array.of_list (List.map lltype_of_ty args_ty)))
  | Rt.ClosureTy (args_ty, ret_ty)
  -> (* { f(i8*, ...)*, i8* } *)
     (Llvm.struct_type ctx [|
        Llvm.pointer_type (* f(i8*, ...)* *)
          (Llvm.function_type (* f(i8*, ...) *)
            (lltype_of_ty ret_ty)
            (Array.of_list
              ((Llvm.pointer_type (Llvm.i8_type ctx)) (* i8* *) ::
               (List.map lltype_of_ty args_ty))));    (* ... *)
        Llvm.pointer_type (Llvm.i8_type ctx) (* i8* *)
      |])
  | _
  -> failwith ("lltype_of_ty: " ^ ((Rt.inspect_value ty) :> string))

type env_map = {
  e_parent  : env_map option;
  e_content : Unicode.utf8s list;
  e_lltype  : Llvm.lltype;
}
let env_maps = Rt.EnvTytbl.create 10

let rec env_map_of_local_env_ty ty =
  try
    Rt.EnvTytbl.find env_maps ty
  with Not_found ->
    let parent_map = Option.map env_map_of_local_env_ty ty.Rt.e_ty_parent in
    (* Serialize the environment. Rt.local_env_ty uses a hash table, which
       is inherently unordered, and LLVM requires an ordered collection. *)
    let content    = Table.map_list ty.Rt.e_ty_bindings ~f:(fun name binding ->
                          name, lltype_of_ty binding.Rt.b_ty)
    in
    let names   = List.map fst content in
    let lltypes = List.map snd content in
    (* Prepend LLVM type for the pointer to the parent environment, if it
       exists. *)
    let lltypes =
      match parent_map with
      | Some map -> Llvm.pointer_type map.e_lltype :: lltypes
      | None     -> lltypes
    in
    let env_map = {
      e_parent  = parent_map;
      e_content = names;
      e_lltype  = Llvm.struct_type ctx (Array.of_list lltypes);
    }
    in
    (* Memoize the environment. *)
    Rt.EnvTytbl.add env_maps ty env_map;
    env_map

let env_map_of_ty ty =
  match ty with
  | Rt.EnvironmentTy ty -> env_map_of_local_env_ty ty
  | _ -> assert false

let local_var_index env_map name =
  (* Find the index of variable in the map content by name. *)
  let index, _ = List.findi (fun i -> (=) name) env_map.e_content in
  (* If this environment has a parent, shift the index by 1 to
     accomodate for its pointer. *)
  match env_map.e_parent with
  | Some _ -> index + 1
  | None   -> index

let rec llconst_of_value llmod heap value =
  let memoize lltype f =
    try
      Rt.Valuetbl.find heap value
    with Not_found ->
      let llglobal = Llvm.declare_global lltype "" llmod in
      let llvalue  = (f llglobal) in
        Llvm.set_initializer llvalue llglobal;
        Rt.Valuetbl.add heap value llglobal;
        llglobal
  in
  match value with
  | Rt.Nil
  -> Llvm.const_struct ctx [||]
  | Rt.Truth
  -> Llvm.const_int (Llvm.integer_type ctx 1) 1
  | Rt.Lies
  -> Llvm.const_int (Llvm.integer_type ctx 1) 0
  | Rt.Unsigned (width, ivalue)
  | Rt.Signed (width, ivalue)
  -> (let llty = Llvm.integer_type ctx width in
      if is_int_big_int ivalue then
        (* Fast path. 31/63 bits. *)
        Llvm.const_int llty (int_of_big_int ivalue)
      else try
        (* Slow path. 64 bits. *)
        let sext =
          match value with
          | Rt.Unsigned _ -> false
          | Rt.Signed   _ -> true
          | _             -> assert false
        in
        Llvm.const_of_int64 llty (int64_of_big_int ivalue) sext
      with Failure _ ->
        (* Very slow path. > 64 bits.
           There is no direct big_int -> llvalue converter. *)
        Llvm.const_int_of_string llty ((string_of_big_int ivalue) :> string) 10)
  | Rt.Tuple xs
  -> Llvm.const_struct ctx (Array.of_list (List.map (llconst_of_value llmod heap) xs))
  | Rt.Record xs
  -> Llvm.const_struct ctx (Array.of_list
      (Table.map_list ~ordered:true ~f:(fun _ -> llconst_of_value llmod heap) xs))
  | Rt.Environment env
  -> (let env_map = env_map_of_ty (Rt.type_of_value value) in
      memoize env_map.e_lltype (fun _ ->
        (* Convert the environment to an LLVM record in a way which is
           compatible with the map env_map_of_ty returns. *)
        let content =
          List.map (fun name ->
              llconst_of_value llmod heap (Table.get_exn env.Rt.e_bindings name).Rt.b_value)
            env_map.e_content
        in
        (* Recur and convert parent environment as well. *)
        let parent = Option.map (fun parent ->
                          llconst_of_value llmod heap (Rt.Environment parent))
                        env.Rt.e_parent in
        (* Prepend parent environment to the structure content, if it exists. *)
        let content =
          match parent with
          | Some parent -> parent :: content
          | None -> content
        in
        Llvm.const_struct ctx (Array.of_list content)))
  | Rt.Package pkg
  -> (let metaklass = Rt.Class (pkg.Rt.p_metaclass, Table.create []) in
      let llty = lltype_of_ty ~ptr:false metaklass in
      memoize llty (fun value ->
        Llvm.set_value_name ("package." ^ (pkg.Rt.p_name :> string)) value;
        Llvm.const_named_struct llty [||]))
  | Rt.Class (klass, specz)
  -> (let llty = lltype_of_ty ~ptr:false (Rt.type_of_value value) in
      memoize llty (fun value ->
        Llvm.set_value_name ("class." ^ (klass.Rt.k_name :> string)) value;
        Llvm.const_named_struct llty [||]))
  | _
  -> failwith ("llconst_of_value: " ^ ((Rt.inspect_value value) :> string))

let gen_proto llmod funcn =
  let name = (funcn.Ssa.id :> string) in
  let args_ty, ret_ty = Ssa.func_ty funcn in
  (* Map FSSA prototype to LLVM prototype. *)
  let llargs_ty = Array.of_list (List.map lltype_of_ty args_ty)
  and llret_ty  = lltype_of_ty ret_ty in
  let llty      = Llvm.function_type llret_ty llargs_ty in
    (* Lookup or create a function with the specified name and
       verify that its prototype matches. *)
    match Llvm.lookup_function name llmod with
    | None -> Llvm.declare_function name llty llmod
    | Some f ->
      assert (Llvm.block_begin f = Llvm.At_end f);
      assert (Llvm.element_type (Llvm.type_of f) == llty);
      f

let gen_llfunc llmod name args ret =
  match Llvm.lookup_function name llmod with
  | None   -> Llvm.declare_function name (Llvm.function_type ret args) llmod
  | Some f -> f

let rec llblit builder src src_idx dst dst_idx len =
  let llval = Llvm.build_extractvalue src src_idx "" builder in
  let dst   = Llvm.build_insertvalue dst llval dst_idx "" builder in
  let len   = len - 1 in
  if len > 0 then llblit builder src (src_idx + 1) dst (dst_idx + 1) len
  else dst

let rec gen_func llmod heap funcn =
  let llconst_of_value = llconst_of_value llmod heap in

  let llfunc  = gen_proto llmod funcn in
  let builder = Llvm.builder ctx in

  (* Define a map between FSSA names and LLVM names *)
  let names   = Ssa.Nametbl.create 10 in

  (* Define list of LLVM phis which need to be fixed up
     and FSSA names of incoming values. *)
  let fixups  = ref [] in

  (* Map FSSA name to LLVM name, and memoize the mapping. *)
  let map name =
    try
      Ssa.Nametbl.find names name
    with Not_found ->
      match name.Ssa.opcode with
      | Ssa.Const value
      -> (* This is an FSSA constant, convert it to an LLVM constant. *)
         (let llvalue = llconst_of_value value in
          Ssa.Nametbl.add names name llvalue;
          llvalue)
      | Ssa.Function _
      -> (* This is an FSSA function, which is semantically equivalent to
            a constant within this context, but has different lookup rules.
            Either look it up, or emit a prototype and hopefully fill it
            later in gen_func. *)
         (match Llvm.lookup_function (name.Ssa.id :> string) llmod with
          | Some llfunc -> llfunc
          | None -> gen_proto llmod name)
      | _
      -> failwith ("gen_func/map " ^ (name.Ssa.id :> string))
  in

  (* Map FSSA primitive to LLVM primitive. *)
  let gen_prim instr id prim operands =
    let module Icmp = Llvm.Icmp in
    match prim, operands with
    (* Debug primitives. *)
    | "debug", [operand]
    -> (match Llvm.lookup_function "__fy_debug" llmod with
        | Some lldebug -> Llvm.build_call lldebug [| map operand |] "" builder
        | None -> assert false)
    | "putchar", [operand]
    -> (let llputchar = gen_llfunc llmod "putchar"
                          [| Llvm.i32_type ctx |] (Llvm.i32_type ctx) in
        Llvm.build_call llputchar [| map operand |] "" builder)

    (* Integer operations. *)
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

    (* Tuple operations. *)
    | "tup_make", _
    -> (let xs_ty = List.map (fun x -> x.Ssa.ty) operands in
        let llty  = Llvm.struct_type ctx (Array.of_list
                        (List.map lltype_of_ty xs_ty)) in
        snd (List.fold_left (fun (idx, lltup) x ->
            idx + 1, Llvm.build_insertvalue lltup (map x) idx "" builder)
          (0, Llvm.undef llty) operands))

    | "tup_extend", [tup; x]
    -> (let lltup, llx = map tup, map x in
        let lltys  = Llvm.struct_element_types (Llvm.type_of lltup) in
        let llty'  = Llvm.struct_type ctx (Array.append lltys
                            (Array.of_list [ Llvm.type_of llx ])) in
        let lltup' = llblit builder lltup 0 (Llvm.undef llty') 0 (Array.length lltys) in
        Llvm.build_insertvalue lltup' llx (Array.length lltys) "" builder)

    | "tup_concat", [lft; rgt]
    -> (let lllft, llrgt = map lft, map rgt in
        let lllfttys = Llvm.struct_element_types (Llvm.type_of lllft) in
        let llrgttys = Llvm.struct_element_types (Llvm.type_of llrgt) in
        let lltys  = Array.append lllfttys llrgttys in
        let llty'  = Llvm.struct_type ctx lltys in
        let lltup  = llblit builder lllft 0 (Llvm.undef llty') 0 (Array.length lllfttys) in
        let rgtidx = Array.length lllfttys in
        llblit builder llrgt 0 lltup rgtidx (Array.length llrgttys))

    | "tup_slice",  [tup; { Ssa.opcode = Ssa.Const (Rt.Integer lft) };
                          { Ssa.opcode = Ssa.Const (Rt.Integer rgt) }]
    -> (let lft    = int_of_big_int lft
        and rgt    = int_of_big_int rgt
        and lltup  = map tup in
        let lltys  = Llvm.struct_element_types (Llvm.type_of lltup) in
        let llty'  = Llvm.struct_type ctx (Array.sub lltys lft (rgt - lft)) in
        llblit builder lltup lft (Llvm.undef llty') 0 (rgt - lft))

    | "tup_index",  [tup; { Ssa.opcode = Ssa.Const (Rt.Integer idx) }]
    -> Llvm.build_extractvalue (map tup) (int_of_big_int idx) "" builder

    (* Closure operations. *)
    | "lam_call", closure :: operands
    -> (* Construct a call instruction with environment substitution. The
          closure is a value type: forall f. { f*, i8* }, where f is the type
          of closure without prepended environment argument, and i8* is the
          generalized type for all environments.

          See also Ssa.CallInstr branch. *)
       (let llclosure = map closure in
        let llfunc    = Llvm.build_extractvalue llclosure 0 "" builder in
        let llenv     = Llvm.build_extractvalue llclosure 1 "" builder in
        let id = if instr.Ssa.ty = Rt.NilTy then "" else id in
        Llvm.build_call llfunc (Array.of_list (llenv :: (List.map map operands))) id builder)

    (* Object operations. *)
    | "obj_alloc", [ cls ]
    -> (* For reference types, allocate an object in system heap (for now).
          For value types, create an object filled with undef. *)
       (match instr.Ssa.ty with
        (* obj_alloc ignors its operand, which only serves a purpose for type
           propagation, and uses its return type to determine an object of which
           type it should return (duh). *)
        | Rt.Class(klass, _) as cls
        -> (let llty = lltype_of_ty cls in
            if klass.Rt.k_is_value then
              Llvm.undef llty
            else
              let llmalloc = gen_llfunc llmod "malloc"
                                [| Llvm.i64_type ctx |]
                                (Llvm.pointer_type (Llvm.i8_type ctx)) in
              let llobj = Llvm.build_call llmalloc [| Llvm.size_of llty |] "" builder in
              Llvm.build_bitcast llobj llty id builder)
        | _
        -> assert false)

    | _, _
    -> failwith ("gen_func/gen_prim: " ^ prim ^
                 "/" ^ (string_of_int (List.length operands)))
  in

  (* Build LLVM instruction out of FSSA instruction. *)
  let gen_instr instr =
    let id = (instr.Ssa.id :> string) in
    let llvalue =
      match instr.Ssa.opcode with
      (* Constants are handled within `map'. *)
      | Ssa.Const _ | Ssa.Function _
      -> assert false

      (* Terminator instructions. *)
      | Ssa.JumpInstr blockn
      -> Llvm.build_br (Llvm.block_of_value (Ssa.Nametbl.find names blockn)) builder
      | Ssa.JumpIfInstr (condn, truen, falsen)
      -> Llvm.build_cond_br (map condn)
              (Llvm.block_of_value (map truen))
              (Llvm.block_of_value (map falsen))
            builder
      | Ssa.ReturnInstr valuen
      -> Llvm.build_ret (map valuen) builder

      (* Phis. *)
      | Ssa.PhiInstr operands
      -> (* Divide all FSSA incoming values into predecessor values
            and successor values. It is assumed here that the basic blocks
            are sorted in dataflow order, therefore all the names which
            are already converted to LLVM come from preceding blocks.

            Constants and functions (which are constants) are always
            accessible. *)
         (let pred, succ = List.partition (fun (_, op) ->
                              match op.Ssa.opcode with
                              | Ssa.Const _ | Ssa.Function _ -> true
                              | _ -> Ssa.Nametbl.mem names op) operands
          in
          (* Build an LLVM phi with existing values. Attempting to pass []
             to build_phi results in segfault :( *)
          let incoming = List.map (fun (block, op) ->
                            map op, Llvm.block_of_value (map block)) pred in
          let llphi    = Llvm.build_phi incoming id builder in
          (* Add all not yet existing values into the fixup list. *)
          List.iter (fun (block, op) -> fixups := (llphi, block, op) :: !fixups) succ;
          llphi)

      (* Local variables. *)
      | Ssa.FrameInstr nextn
      -> (let env_map = env_map_of_ty instr.Ssa.ty in
          (* Allocate an environment for this function. *)
          let llenv = Llvm.build_alloca env_map.e_lltype id builder in
          (* Fill in the next-environment slot. It is always available,
             because an environment in FSSA is always chained to another one. *)
          let llparentptr = Llvm.build_struct_gep llenv 0 "" builder in
          let llnextty    = Llvm.pointer_type (Option.get env_map.e_parent).e_lltype in
          let llnext      = Llvm.build_bitcast (map nextn) llnextty "" builder in
          ignore (Llvm.build_store llnext llparentptr builder);
          (* Return the newly built environment. *)
          llenv)

      | Ssa.LVarLoadInstr (envn, var)
      | Ssa.LVarStoreInstr (envn, var, _)
      -> (let env_map = env_map_of_ty envn.Ssa.ty in
          (* Get a pointer to the variable in the environment. *)
          let rec lookup env_map llenv =
            try
              (* Try to lookup on the current level. This possibly
                 raises Not_found. *)
              let index = local_var_index env_map var in
              (* Found it, do the GEP. *)
              let llvar = Llvm.build_struct_gep llenv index "" builder in
              llvar
            with Not_found ->
              match env_map.e_parent with
              | Some env_map ->
                (* The 0th element in the environment is the parent
                   environment pointer. *)
                let llenvptr = Llvm.build_struct_gep llenv 0 "" builder in
                let llenv    = Llvm.build_load llenvptr "" builder in
                (* Recursively look up at the upper level. *)
                lookup env_map llenv
              | None -> assert false
          in
          let llenvty = Llvm.pointer_type env_map.e_lltype in
          (* Environments are always represented as i8*: their true type is
             hidden. Sort of an existential type for LLVM IR. *)
          let llenv   = Llvm.build_bitcast (map envn) llenvty "" builder in
          let llvar   = lookup env_map llenv in
          (* Actually load or store the variable value. *)
          match instr.Ssa.opcode with
          | Ssa.LVarLoadInstr (_, _)
          -> Llvm.build_load llvar id builder
          | Ssa.LVarStoreInstr (_, _, value)
          -> Llvm.build_store (map value) llvar builder
          | _ -> assert false)

      (* Instance variables. *)
      | Ssa.IVarLoadInstr (obj, var)
      | Ssa.IVarStoreInstr (obj, var, _)
      -> (let klass =
            match obj.Ssa.ty with
            | Rt.Class (klass, _) -> klass
            | _ -> assert false
          in
          if klass.Rt.k_is_value then
            assert false
          else
            (* Build a getelementptr instruction for the field. *)
            let rec lookup klass indices =
              try
                let index, _ =
                  List.findi (fun idx (name, _) -> var = name) klass.Rt.k_slots
                in
                match klass.Rt.k_ancestor with
                | Some _ -> (index + 1) :: indices
                | None   -> index :: indices
              with Not_found ->
                match klass.Rt.k_ancestor with
                | Some ancestor -> lookup ancestor (0 :: indices)
                | None -> assert false
            in
            let lli32ty = Llvm.i32_type ctx in
            let indices = List.rev (lookup klass [0]) in
            let indices = List.map (Llvm.const_int lli32ty) indices in
            let llgep   = Llvm.build_in_bounds_gep (map obj) (Array.of_list indices)
                              ((var :> string) ^ ".p") builder in
            (* Actually load or store the slot value. *)
            match instr.Ssa.opcode with
            | Ssa.IVarLoadInstr (_, _)
            -> Llvm.build_load llgep id builder
            | Ssa.IVarStoreInstr (_, _, value)
            -> Llvm.build_store (map value) llgep builder
            | _ -> assert false)

      (* Functions and closures. *)
      | Ssa.CallInstr (funcn, operands)
      -> (* Construct a call instruction. The instruction has the same type
            as the result type of the function it calls, so if that's nil, make
            it unnamed: otherwise LLVM will reject the module. *)
         (let id = if instr.Ssa.ty = Rt.NilTy then "" else id in
          Llvm.build_call (map funcn) (Array.of_list (List.map map operands)) id builder)
      | Ssa.ClosureInstr (callee, env)
      -> (* See also lam_call primitive. *)
         (let llfunc, llenv = map callee, map env in
          let llenv = Llvm.build_bitcast llenv (Llvm.pointer_type (Llvm.i8_type ctx)) "" builder in
          let llclosurety = Llvm.struct_type ctx [|
                              Llvm.type_of llfunc;
                              Llvm.type_of llenv;
                            |] in
          let llclosure = Llvm.undef llclosurety in
          let llclosure = Llvm.build_insertvalue llclosure llfunc 0 "" builder in
          let llclosure = Llvm.build_insertvalue llclosure llenv  1 "" builder in
          llclosure)

      (* Primitives. *)
      | Ssa.PrimitiveInstr (prim, operands)
      -> gen_prim instr id (prim :> string) operands

      | _
      -> assert false
    in
    Ssa.Nametbl.add names instr llvalue
  in

  let func    = Ssa.func_of_name funcn in

  (* Rename the LLVM arguments to match FSSA arguments,
     and add them to the value map. *)
  let llparams = Llvm.params llfunc in
  List.iteri (fun index argn ->
      let llparam = llparams.(index) in
      Ssa.Nametbl.add names argn llparam;
      Llvm.set_value_name (argn.Ssa.id :> string) llparam)
    func.Ssa.arguments;

  (* Create LLVM basic blocks for corresponding FSSA basic
     blocks. *)
  List.iter (fun blockn ->
      let llblock = Llvm.append_block ctx (blockn.Ssa.id :> string) llfunc in
      Ssa.Nametbl.add names blockn (Llvm.value_of_block llblock))
    func.Ssa.basic_blocks;

  (* Codegen non-phi instructions while traversing blocks in
     domination order. *)
  List.iter (fun blockn ->
      let block   = Ssa.block_of_name blockn in
      let llblock = Llvm.block_of_value (Ssa.Nametbl.find names blockn) in
      Llvm.position_at_end llblock builder;
      List.iter gen_instr block.Ssa.instructions)
    func.Ssa.basic_blocks;

  (* Fixup phi instructions. *)
  List.iter (fun (llphi, pred, name) ->
      let llblock = Llvm.block_of_value (Ssa.Nametbl.find names pred) in
      let llvalue = map name in
      Llvm.add_incoming (llvalue, llblock) llphi)
    !fixups;

  (* Validate the result. *)
  if not (Llvm_analysis.verify_function llfunc) then begin
    Llvm.dump_module llmod;
    Llvm_analysis.assert_valid_function llfunc
  end;

  llfunc

let llvm_module_of_ssa_capsule capsule =
  let llmod = Llvm.create_module ctx "foundry" in
  let heap  = Rt.Valuetbl.create 10 in
    ignore (gen_debug llmod);
    Ssa.iter_funcs capsule ~f:(fun funcn ->
      ignore (gen_func llmod heap funcn));
    llmod
