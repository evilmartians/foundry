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
  let memoize ~ptr name generate =
    let llty =
      try
        Rt.Valuetbl.find types ty
      with Not_found ->
        (* Create and memoize a named struct type. *)
        let llty = Llvm.named_struct_type ctx name in
        Rt.Valuetbl.add types ty llty;
        (* Populate the contents of the type. *)
        let is_packed, elts = generate () in
        Llvm.struct_set_body llty (Array.of_list elts) is_packed;
        (* Return newly defined type. *)
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
  | Rt.OptionTy x
  -> (let llty = lltype_of_ty x in
      if Llvm.classify_type llty = Llvm.TypeKind.Pointer then
        llty
      else
        Llvm.struct_type ctx [| Llvm.i1_type ctx; llty |])
  | Rt.TupleTy xs
  -> Llvm.struct_type ctx (Array.of_list (List.map lltype_of_ty xs))
  | Rt.RecordTy xs
  -> Llvm.struct_type ctx (Array.of_list
        (Assoc.map_list xs ~f:(fun _ -> lltype_of_ty ~ptr:false)))
  | Rt.ArrayTy _
  -> (memoize ~ptr "array" (fun () ->
        false, [ Llvm.i32_type ctx; Llvm.i32_type ctx;
                 Llvm.pointer_type (Llvm.i8_type ctx) ]))
  | Rt.EnvironmentTy env_ty
  -> Llvm.pointer_type (Llvm.i8_type ctx)
  | Rt.Class (klass, specz)
  -> (let ptr = ptr && (not klass.Rt.k_is_value) in
      memoize ~ptr (klass.Rt.k_name :> string) (fun () ->
        let slots = Assoc.map_list (fun name _ ->
                        lltype_of_ty (Typing.slot_ty (klass, specz) name))
                      klass.Rt.k_ivars
        in
        let slots =
          match klass.Rt.k_ancestor with
          | Some ancestor -> (lltype_of_ty ~ptr:false (Rt.Class (ancestor, specz))) :: slots
          | None -> slots
        in
        false, slots))
  | Rt.FunctionTy (arg_tys, ret_ty)
  -> (Llvm.function_type
        (lltype_of_ty ret_ty)
        (Array.of_list (List.map lltype_of_ty arg_tys)))
  | Rt.LambdaTy (arg_ty_elems, ret_ty)
  -> (let arg_tys = Rt.tys_of_lambda_ty_elems arg_ty_elems in
      (* { f(i8*, ...)*, i8* } *)
      Llvm.struct_type ctx [|
        Llvm.pointer_type (* f(i8*, ...)* *)
          (Llvm.function_type (* f(i8*, ...) *)
            (lltype_of_ty ret_ty)
            (Array.of_list
              ((Llvm.pointer_type (Llvm.i8_type ctx)) (* i8* *) ::
               (List.map lltype_of_ty arg_tys))));    (* ... *)
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
    let content = Table.map_list ty.Rt.e_ty_bindings ~f:(fun name binding ->
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
  | Rt.Option x
  -> (match x with
      | Rt.Empty ty
      -> Llvm.const_null (lltype_of_ty (Rt.OptionTy ty))
      | Rt.Full v
      -> (let llone = Llvm.const_int (Llvm.integer_type ctx 1) 1 in
          Llvm.const_struct ctx [| llone; llconst_of_value llmod heap v |]))
  | Rt.Tuple xs
  -> Llvm.const_struct ctx (Array.of_list (List.map (llconst_of_value llmod heap) xs))
  | Rt.Record xs
  -> Llvm.const_struct ctx (Array.of_list
      (Assoc.map_list xs ~f:(fun _ -> llconst_of_value llmod heap)))
  | Rt.Array (_, x)
  -> (let llty = lltype_of_ty ~ptr:false (Rt.ArrayTy x.Rt.st_ty) in
      memoize llty (fun _ ->
        let const_i32 =
          Llvm.const_int (Llvm.i32_type ctx)
        in
        let llelemty = lltype_of_ty x.Rt.st_ty in
        let llelems  = List.map (llconst_of_value llmod heap) (DynArray.to_list x.Rt.st_elems) in
        let llundefs =
          let rec pad count undefs =
            if count = 0 then undefs
            else pad (count - 1) (Llvm.undef llelemty :: undefs)
          in
          pad (x.Rt.st_capacity - (DynArray.length x.Rt.st_elems)) []
        in
        let llary =
          Llvm.define_global "" (Llvm.const_array llelemty
                                  (Array.of_list (llelems @ llundefs))) llmod
        in
        Llvm.const_named_struct llty [|
          const_i32 x.Rt.st_capacity;
          const_i32 (DynArray.length x.Rt.st_elems);
          Llvm.const_bitcast llary (Llvm.pointer_type (Llvm.i8_type ctx))
        |]))
  | Rt.Environment env
  -> (let env_ty  = Rt.type_of_environment ~imm:false env in
      let env_map = env_map_of_local_env_ty env_ty in
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
  -> (let metaklass = Rt.Class (pkg.Rt.p_metaclass, Assoc.empty) in
      let llty = lltype_of_ty ~ptr:false metaklass in
      memoize llty (fun llvalue ->
        Llvm.set_value_name ("package." ^ (pkg.Rt.p_name :> string)) llvalue;
        Llvm.const_null llty))
  | Rt.Class (klass, specz)
  -> (let llty = lltype_of_ty ~ptr:false (Rt.type_of_value value) in
      memoize llty (fun llvalue ->
        Llvm.set_value_name ("class." ^ (klass.Rt.k_name :> string)) llvalue;
        Llvm.const_null llty))
  | Rt.Instance ({ Rt.i_class = klass, specz; Rt.i_slots = slots; })
  -> (let rec gen_inst klass =
        let elems = Assoc.map_list klass.Rt.k_ivars ~f:(fun name _ ->
                      llconst_of_value llmod heap (Table.get_exn slots name)) in
        let elems =
          match klass.Rt.k_ancestor with
          | Some ancestor -> (gen_inst ancestor) :: elems
          | None -> elems
        in
        let elems = Array.of_list elems in
        Llvm.const_named_struct (lltype_of_ty ~ptr:false (Rt.Class (klass, specz))) elems
      in
      if klass.Rt.k_is_value then
        gen_inst klass
      else
        let ty = lltype_of_ty ~ptr:false (Rt.Class (klass, specz)) in
        memoize ty (fun _ -> gen_inst klass))
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

let gen_llfunc_va llmod name args ret =
  let llty = Llvm.var_arg_function_type ret args in
  match Llvm.lookup_function name llmod with
  | None   -> Llvm.declare_function name llty llmod
  | Some f -> f

let rec llblit builder src src_idx dst dst_idx len =
  if len > 0 then
    let llval = Llvm.build_extractvalue src src_idx "" builder in
    let dst   = Llvm.build_insertvalue dst llval dst_idx "" builder in
    llblit builder src (src_idx + 1) dst (dst_idx + 1) (len - 1)
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
  let lookup name =
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
    let int_binop lhs rhs unsigned_op signed_op =
      match lhs.Ssa.ty, rhs.Ssa.ty with
      | Rt.UnsignedTy(wa), Rt.UnsignedTy(wb) when wa = wb
      -> unsigned_op (lookup lhs) (lookup rhs) id builder
      | Rt.SignedTy(wa),   Rt.SignedTy(wb)   when wa = wb
      -> signed_op   (lookup lhs) (lookup rhs) id builder
      | _
      -> assert false
    in
    match prim, operands with
    (* Debug primitives. *)
    | "debug", [operand]
    -> (let operand = lookup operand in
        let operand = Llvm.build_intcast operand (Llvm.i32_type ctx) "" builder in
        match Llvm.lookup_function "__fy_debug" llmod with
        | Some lldebug -> Llvm.build_call lldebug [| operand |] "" builder
        | None -> assert false)

    (* Boolean operations. *)
    | "bool_neg", [value]
    -> (let i1 = Llvm.i1_type ctx in
        Llvm.build_select (lookup value) (Llvm.const_int i1 0) (Llvm.const_int i1 1) "" builder)

    (* Integer operations. *)
    | "int_add",  [lhs; rhs] -> Llvm.build_add  (lookup lhs) (lookup rhs) id builder
    | "int_sub",  [lhs; rhs] -> Llvm.build_sub  (lookup lhs) (lookup rhs) id builder
    | "int_mul",  [lhs; rhs] -> Llvm.build_mul  (lookup lhs) (lookup rhs) id builder
    | "int_div",  [lhs; rhs] -> int_binop lhs rhs Llvm.build_udiv Llvm.build_sdiv
    | "int_mod",  [lhs; rhs] -> int_binop lhs rhs Llvm.build_urem Llvm.build_srem
    | "int_and",  [lhs; rhs] -> Llvm.build_and  (lookup lhs) (lookup rhs) id builder
    | "int_or",   [lhs; rhs] -> Llvm.build_or   (lookup lhs) (lookup rhs) id builder
    | "int_xor",  [lhs; rhs] -> Llvm.build_xor  (lookup lhs) (lookup rhs) id builder
    | "int_shl",  [lhs; rhs] -> Llvm.build_shl  (lookup lhs) (lookup rhs) id builder
    | "int_shr",  [lhs; rhs] -> int_binop lhs rhs Llvm.build_lshr Llvm.build_ashr
    | "int_eq",   [lhs; rhs] -> Llvm.build_icmp Icmp.Eq  (lookup lhs) (lookup rhs) id builder
    | "int_ne",   [lhs; rhs] -> Llvm.build_icmp Icmp.Ne  (lookup lhs) (lookup rhs) id builder
    | "int_le",   [lhs; rhs] -> int_binop lhs rhs (Llvm.build_icmp Icmp.Ule) (Llvm.build_icmp Icmp.Sle)
    | "int_lt",   [lhs; rhs] -> int_binop lhs rhs (Llvm.build_icmp Icmp.Ult) (Llvm.build_icmp Icmp.Slt)
    | "int_ge",   [lhs; rhs] -> int_binop lhs rhs (Llvm.build_icmp Icmp.Uge) (Llvm.build_icmp Icmp.Sge)
    | "int_gt",   [lhs; rhs] -> int_binop lhs rhs (Llvm.build_icmp Icmp.Ugt) (Llvm.build_icmp Icmp.Sgt)

    (* Option operations. *)
    | "opt_alloc",  [item]
    -> (let llitem   = lookup item in
        let llitemty = Llvm.type_of llitem in
        if Llvm.classify_type llitemty <> Llvm.TypeKind.Pointer then
          let llopt = Llvm.undef (lltype_of_ty instr.Ssa.ty) in
          let llone = Llvm.const_int (Llvm.i1_type ctx) 1 in
          let llopt = Llvm.build_insertvalue llopt llone  0 "" builder in
          let llopt = Llvm.build_insertvalue llopt llitem 1 id builder in
          llopt
        else
          llitem)

    | "opt_any",    [opt]
    -> (let llopt   = lookup opt in
        let lloptty = Llvm.type_of llopt in
        if Llvm.classify_type lloptty <> Llvm.TypeKind.Pointer then
          Llvm.build_extractvalue llopt 0 id builder
        else
          Llvm.build_is_null llopt id builder)

    | "opt_get",    [opt]
    -> (let llopt   = lookup opt in
        let lloptty = Llvm.type_of llopt in
        if Llvm.classify_type lloptty <> Llvm.TypeKind.Pointer then
          Llvm.build_extractvalue llopt 1 id builder
        else
          llopt)

    (* Tuple operations. *)
    | "tup_lookup", [tup; { Ssa.opcode = Ssa.Const (Rt.Integer idx) }]
    -> Llvm.build_extractvalue (lookup tup) (int_of_big_int idx) "" builder

    | "tup_slice",  [tup; { Ssa.opcode = Ssa.Const (Rt.Integer lft) };
                          { Ssa.opcode = Ssa.Const (Rt.Integer rgt) }]
    -> (let lft    = int_of_big_int lft
        and rgt    = int_of_big_int rgt
        and lltup  = lookup tup in
        let lltys  = Llvm.struct_element_types (Llvm.type_of lltup) in
        let llty'  = Llvm.struct_type ctx (Array.sub lltys lft (rgt - lft)) in
        llblit builder lltup lft (Llvm.undef llty') 0 (rgt - lft))

    (* Record operations. *)
    | "rec_lookup", [ { Ssa.ty     = Rt.RecordTy fields } as re;
                      { Ssa.opcode = Ssa.Const (Rt.Symbol field) } ]
    -> Llvm.build_extractvalue (lookup re) (Assoc.index fields field) "" builder

    (* Array operations. *)
    | "ary_capa",   [ary]
    -> (let llary = lookup ary in
        let llptr = Llvm.build_struct_gep llary 0 "" builder in
        Llvm.build_load llptr id builder)

    | "ary_length", [ary]
    -> (let llary = lookup ary in
        let llptr = Llvm.build_struct_gep llary 1 "" builder in
        Llvm.build_load llptr id builder)

    | ("ary_get" | "ary_set"),
                    ({ Ssa.ty = Rt.ArrayTy(elem_ty) } as ary)   ::
                    ({ Ssa.ty = Rt.UnsignedTy(32)   } as index) :: rest
    -> (let llary     = lookup ary
        and llindex   = lookup index
        and llelemty  = lltype_of_ty elem_ty in
        let llstgptr  = Llvm.build_struct_gep llary 2 "" builder in
        let llstg     = Llvm.build_load llstgptr "" builder in
        let llstg     = Llvm.build_bitcast llstg (Llvm.pointer_type llelemty) "" builder in
        let llelemptr = Llvm.build_in_bounds_gep llstg [| llindex |] "" builder in
        match prim, rest with
        | "ary_get", []
        -> Llvm.build_load  llelemptr id builder
        | "ary_set", [value]
        -> Llvm.build_store (lookup value) llelemptr builder
        | _
        -> assert false)

    (* Object operations. *)
    | "obj_alloc", [cls]
    -> (* For reference types, allocate an object in system heap (for now).
          For value types, create an object filled with undef. *)
       (match instr.Ssa.ty with
        (* obj_alloc ignors its operand, which only serves a purpose for type
           propagation, and uses its return type to determine an object of which
           type it should return (duh). *)
        | Rt.Class(klass, _) as cls
        -> (let llty = lltype_of_ty ~ptr:false cls in
            if klass.Rt.k_is_value then
              Llvm.undef llty
            else
              Llvm.build_malloc llty id builder)
        | _
        -> assert false)

    (* Memory manipulation. *)
    | ("mem_load" | "mem_loadv"),
      [ { Ssa.opcode = Ssa.Const (Rt.Unsigned (_, align)) }; addr ]
    -> (let width =
          match instr.Ssa.ty with
          | Rt.UnsignedTy (width) -> width
          | _ -> assert false
        in
        let llret_ty  = Llvm.integer_type ctx width in
        let lladdr    = Llvm.build_inttoptr (lookup addr) (Llvm.pointer_type llret_ty) "" builder in
        let llload    = Llvm.build_load lladdr id builder in
        Llvm.set_instruction_alignment (int_of_big_int align) llload;
        Llvm.set_volatile (prim = "mem_loadv") llload;
        llload)

    | ( "mem_store" | "mem_storev" ),
      [ { Ssa.opcode = Ssa.Const (Rt.Unsigned (_, align)) }; addr; value ]
    -> (let llval     = lookup value in
        let llval_ty  = Llvm.type_of llval in
        let lladdr    = Llvm.build_inttoptr (lookup addr) (Llvm.pointer_type llval_ty) "" builder in
        let llstore   = Llvm.build_store llval lladdr builder in
        Llvm.set_instruction_alignment (int_of_big_int align) llstore;
        Llvm.set_volatile (prim = "mem_storev") llstore;
        llconst_of_value Rt.Nil)

    (* Calling C functions. *)
    | "external", { Ssa.opcode = Ssa.Const (Rt.Symbol ext_func) } :: args
    -> (let llargs    = List.map lookup args in
        let llargs_ty = Array.of_list (List.map Llvm.type_of llargs) in
        let llret_ty  = lltype_of_ty instr.Ssa.ty in
        let llfunc    = gen_llfunc llmod (ext_func :> string) llargs_ty llret_ty in
        Llvm.build_call llfunc (Array.of_list llargs) "" builder)

    | "externalva", { Ssa.opcode = Ssa.Const (Rt.Symbol ext_func) } ::
                    { Ssa.opcode = Ssa.Const (Rt.Integer arg_cnt) } :: args
    -> (let arg_cnt   = int_of_big_int arg_cnt in
        let llargs, llrest = List.split_nth arg_cnt (List.map lookup args) in
        let llargs_ty = Array.of_list (List.map Llvm.type_of llargs) in
        let llret_ty  = lltype_of_ty instr.Ssa.ty in
        let llfunc    = gen_llfunc_va llmod (ext_func :> string) llargs_ty llret_ty in
        Llvm.build_call llfunc (Array.of_list (llargs @ llrest)) "" builder)

    | _, _
    -> failwith ("gen_func/gen_prim: " ^ prim ^
                 "/" ^ (string_of_int (List.length operands)))
  in

  (* Build LLVM instruction out of FSSA instruction. *)
  let gen_instr instr =
    let id = (instr.Ssa.id :> string) in
    let llvalue =
      match instr.Ssa.opcode with
      (* Constants are handled within `lookup'. *)
      | Ssa.Const _ | Ssa.Function _
      -> assert false

      (* Terminator instructions. *)
      | Ssa.JumpInstr blockn
      -> Llvm.build_br (Llvm.block_of_value (Ssa.Nametbl.find names blockn)) builder
      | Ssa.JumpIfInstr (condn, truen, falsen)
      -> Llvm.build_cond_br (lookup condn)
              (Llvm.block_of_value (lookup truen))
              (Llvm.block_of_value (lookup falsen))
            builder
      | Ssa.ReturnInstr valuen
      -> Llvm.build_ret (lookup valuen) builder

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
                            lookup op, Llvm.block_of_value (lookup block)) pred in
          let llphi    = Llvm.build_phi incoming id builder in
          (* Add all not yet existing values into the fixup list. *)
          List.iter (fun (block, op) -> fixups := (llphi, block, op) :: !fixups) succ;
          llphi)

      (* Select. *)
      | Ssa.SelectInstr (condn, truen, falsen)
      -> Llvm.build_select (lookup condn) (lookup truen) (lookup falsen) id builder

      (* Local variables. *)
      | Ssa.FrameInstr nextn
      -> (let env_map = env_map_of_ty instr.Ssa.ty in
          (* Allocate an environment for this function. *)
          let llenv = Llvm.build_alloca env_map.e_lltype id builder in
          (* Fill in the next-environment slot. It is always available,
             because an environment in FSSA is always chained to another one. *)
          let llparentptr = Llvm.build_struct_gep llenv 0 "" builder in
          let llnextty    = Llvm.pointer_type (Option.get env_map.e_parent).e_lltype in
          let llnext      = Llvm.build_bitcast (lookup nextn) llnextty "" builder in
          ignore (Llvm.build_store llnext llparentptr builder);
          (* Return the newly built environment. *)
          llenv)

      | Ssa.LVarLoadInstr (envn, var)
      | Ssa.LVarStoreInstr (envn, var, _)
      -> (let env_map = env_map_of_ty envn.Ssa.ty in
          (* Get a pointer to the variable in the environment. *)
          let rec lookup_lvar env_map llenv =
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
                lookup_lvar env_map llenv
              | None -> assert false
          in
          let llenvty = Llvm.pointer_type env_map.e_lltype in
          (* Environments are always represented as i8*: their true type is
             hidden. Sort of an existential type for LLVM IR. *)
          let llenv   = Llvm.build_bitcast (lookup envn) llenvty "" builder in
          let llvar   = lookup_lvar env_map llenv in
          (* Actually load or store the variable value. *)
          match instr.Ssa.opcode with
          | Ssa.LVarLoadInstr (_, _)
          -> Llvm.build_load llvar id builder
          | Ssa.LVarStoreInstr (_, _, value)
          -> Llvm.build_store (lookup value) llvar builder
          | _ -> assert false)

      (* Instance variables. *)
      | Ssa.IVarLoadInstr  ({ Ssa.ty = Rt.Class(klass, _) } as obj, var)
      | Ssa.IVarStoreInstr ({ Ssa.ty = Rt.Class(klass, _) } as obj, var, _)
        when not klass.Rt.k_is_value
      -> (* Build a getelementptr instruction for the field. *)
         (let rec lookup_ivar klass indices =
            try
              let index = Assoc.index klass.Rt.k_ivars var in
              match klass.Rt.k_ancestor with
              | Some _ -> (index + 1) :: indices
              | None   -> index :: indices
            with Not_found ->
              match klass.Rt.k_ancestor with
              | Some ancestor -> lookup_ivar ancestor (0 :: indices)
              | None -> assert false
          in
          let lli32ty = Llvm.i32_type ctx in
          let indices = List.rev (lookup_ivar klass [0]) in
          let indices = List.map (Llvm.const_int lli32ty) indices in
          let llgep   = Llvm.build_in_bounds_gep (lookup obj) (Array.of_list indices)
                            ((var :> string) ^ ".p") builder in
          (* Actually load or store the slot value. *)
          match instr.Ssa.opcode with
          | Ssa.IVarLoadInstr (_, _)
          -> Llvm.build_load llgep id builder
          | Ssa.IVarStoreInstr (_, _, value)
          -> Llvm.build_store (lookup value) llgep builder
          | _ -> assert false)

      | Ssa.IVarLoadInstr  ({ Ssa.ty = Rt.Class(klass, _) } as obj, var)
        when klass.Rt.k_is_value
      -> (* Build an extractvalue chain for the field. *)
         (let rec load klass llagg =
            try
              let index = Assoc.index klass.Rt.k_ivars var in
              let index =
                match klass.Rt.k_ancestor with
                | Some _ -> index + 1
                | None   -> index
              in
              Llvm.build_extractvalue llagg index "" builder
            with Not_found ->
              match klass.Rt.k_ancestor with
              | Some ancestor
              -> (let llagg = Llvm.build_extractvalue llagg 0 "" builder in
                  load ancestor llagg)
              | None
              -> assert false
          in
          load klass (lookup obj))

      | Ssa.IVarStoreInstr ({ Ssa.ty = Rt.Class(klass, _) } as obj, var, value)
        when klass.Rt.k_is_value
      -> (* Build an insertvalue chain for the field. *)
         (let rec store klass llagg llval =
            try
              let index = Assoc.index klass.Rt.k_ivars var in
              let index =
                match klass.Rt.k_ancestor with
                | Some _ -> index + 1
                | None   -> index
              in
              Llvm.build_insertvalue llagg llval index "" builder
            with Not_found ->
              match klass.Rt.k_ancestor with
              | Some ancestor
              -> (let llinner = Llvm.build_extractvalue llagg 0 "" builder in
                  let llinner = store ancestor llinner llval in
                  Llvm.build_insertvalue llagg llinner 0 "" builder)
              | None
              -> assert false
          in
          store klass (lookup obj) (lookup value))

      (* Functions and closures. *)
      | Ssa.CallInstr ({ Ssa.ty } as callee, operands)
      -> (match ty with
          | Rt.FunctionTy (_, ret_ty)
          -> (* Construct a call instruction. The instruction has the same type
                as the result type of the function it calls, so if that's nil, make
                it unnamed: otherwise LLVM will reject the module. *)
             (let id = if ret_ty = Rt.NilTy then "" else id in
              Llvm.build_call (lookup callee) (Array.of_list (List.map lookup operands)) id builder)
          | Rt.LambdaTy (_, ret_ty)
          -> (* Construct a call instruction with environment substitution. The
                closure is a value type: forall f. { f*, i8* }, where f is the type
                of closure without prepended environment argument, and i8* is the
                generalized type for all environments. *)
             (let llclosure = lookup callee in
              let llfunc    = Llvm.build_extractvalue llclosure 0 "" builder in
              let llenv     = Llvm.build_extractvalue llclosure 1 "" builder in
              let id        = if ret_ty = Rt.NilTy then "" else id in
              Llvm.build_call llfunc (Array.of_list (llenv :: (List.map lookup operands))) id builder)
          | _
          -> assert false)

      | Ssa.ClosureInstr (callee, env)
      -> (let llfunc, llenv = lookup callee, lookup env in
          let llenv = Llvm.build_bitcast llenv (Llvm.pointer_type (Llvm.i8_type ctx)) "" builder in
          let llclosurety = Llvm.struct_type ctx [|
                              Llvm.type_of llfunc;
                              Llvm.type_of llenv;
                            |] in
          let llclosure = Llvm.undef llclosurety in
          let llclosure = Llvm.build_insertvalue llclosure llfunc 0 "" builder in
          let llclosure = Llvm.build_insertvalue llclosure llenv  1 "" builder in
          llclosure)

      (* Tuples. *)
      | Ssa.TupleExtendInstr (tup, xs)
      -> (let lltup  = lookup tup in
          let llxs   = List.map lookup xs in
          let lltys  = Llvm.struct_element_types (Llvm.type_of lltup) in
          let llty'  = Llvm.struct_type ctx (Array.append lltys
                          (Array.of_list (List.map Llvm.type_of llxs))) in
          let lltup' = llblit builder lltup 0 (Llvm.undef llty') 0 (Array.length lltys) in
          snd (List.fold_left (fun (idx, lltup) x ->
              idx + 1, Llvm.build_insertvalue lltup (lookup x) idx "" builder)
            (Array.length lltys, lltup') xs))

      | Ssa.TupleConcatInstr (lft, rgt)
      -> (let lllft, llrgt = lookup lft, lookup rgt in
          let lllfttys = Llvm.struct_element_types (Llvm.type_of lllft) in
          let llrgttys = Llvm.struct_element_types (Llvm.type_of llrgt) in
          let lltys  = Array.append lllfttys llrgttys in
          let llty'  = Llvm.struct_type ctx lltys in
          let lltup  = llblit builder lllft 0 (Llvm.undef llty') 0 (Array.length lllfttys) in
          let rgtidx = Array.length lllfttys in
          llblit builder llrgt 0 lltup rgtidx (Array.length llrgttys))

      (* Records. *)
      | Ssa.RecordExtendInstr (({ Ssa.ty = Rt.RecordTy lfttys } as lft),
                               operands)
      -> (let operands =
            Assoc.sorted (List.map (fun (key, value) ->
                match key.Ssa.opcode with
                | Ssa.Const (Rt.Symbol name) -> name, lookup value
                | _ -> assert false)
              operands)
          and xs =
            match instr.Ssa.ty with
            | Rt.RecordTy xs -> xs
            | _ -> assert false
          and lllft = lookup lft
          and llty  = lltype_of_ty instr.Ssa.ty in
          let llre  = Llvm.undef llty in
          Assoc.fold llre xs ~f:(fun name llre _ ->
            let llval =
              if Assoc.mem operands name then
                Assoc.find operands name
              else
                Llvm.build_extractvalue lllft (Assoc.index lfttys name) "" builder
            in
            Llvm.build_insertvalue llre llval (Assoc.index xs name) "" builder))

      | Ssa.RecordConcatInstr (({ Ssa.ty = Rt.RecordTy lfttys } as lft),
                               ({ Ssa.ty = Rt.RecordTy rgttys } as rgt))
      -> (let xs =
            match instr.Ssa.ty with
            | Rt.RecordTy xs -> xs
            | _ -> assert false
          in
          let lllft, llrgt = lookup lft, lookup rgt in
          let llty = lltype_of_ty instr.Ssa.ty in
          let llre = Llvm.undef llty in
          Assoc.fold llre xs ~f:(fun name llre _ ->
            let llval =
              if Assoc.mem rgttys name then
                Llvm.build_extractvalue llrgt (Assoc.index rgttys name) "" builder
              else
                Llvm.build_extractvalue lllft (Assoc.index lfttys name) "" builder
            in
            Llvm.build_insertvalue llre llval (Assoc.index xs name) "" builder))

      (* Primitives. *)
      | Ssa.PrimitiveInstr (prim, operands)
      -> gen_prim instr id (prim :> string) operands

      | _
      -> (IrPrinter.print_name instr;
          failwith "Cannot lower instruction")
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
      let llvalue = lookup name in
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
