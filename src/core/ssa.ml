open Unicode.Std
open ExtList

module rec NameType :
sig
  type name = {
    mutable id        : string;
    mutable ty        : Rt.ty;
    mutable opcode    : opcode;
    mutable location  : Location.t;

    (* Internal fields *)
    mutable n_parent  : name_parent;
    mutable n_uses    : name list;
            n_hash    : int;
  }
  and name_parent =
  | ParentNone
  | ParentCapsule    of capsule
  | ParentFunction   of name
  | ParentBasicBlock of name
  and capsule = {
    mutable functions    : name list;
            overloads    : overloads;
            lambda_cache : lambda_cache;
            c_symtab     : Symtab.t;
  }
  and overloads    = (Rt.ty * name) Nametbl.t
  and lambda_cache = name Lambdatbl.t
  and func = {
            name         : string;
    mutable arguments    : name list;
    mutable basic_blocks : name list;
            f_symtab     : Symtab.t;
    mutable f_original   : name option;
  }
  and basic_block = {
    mutable instructions : name list;
  }
  and opcode =
  | InvalidInstr
  (* Functions *)
  | Function          of func
  | Argument
  | BasicBlock        of basic_block
  (* Constants *)
  | Const             of Rt.value
  (* Terminators *)
  | JumpInstr         of (*target*) name
  | JumpIfInstr       of (*condition*) name * (*if_true*) name * (*if_false*) name
  | ReturnInstr       of (*value*) name
  (* Other *)
  | PhiInstr          of ((*basic_block*) name * (*value*) name) list
  | SelectInstr       of (*condition*) name * (*if_true*) name * (*if_false*) name
  (* Language-specific opcodes *)
  | FrameInstr        of (*parent*) name
  | LVarLoadInstr     of (*environment*) name * (*name*) string
  | LVarStoreInstr    of (*environment*) name * (*name*) string * (*value*) name
  | IVarLoadInstr     of (*object*) name  * (*name*) string
  | IVarStoreInstr    of (*object*) name  * (*name*) string * (*value*) name
  | CallInstr         of (*func*) name    * (*operands*) name list
  | ClosureInstr      of (*func*) name    * (*environment*) name
  | SpecializeInstr   of (*type*) name    * name Assoc.sorted_t
  | TupleExtendInstr  of (*tuple*) name   * (*elems*) name list
  | TupleConcatInstr  of (*tuple*) name   * (*tuple*) name
  | RecordExtendInstr of (*record*) name  * (*elems*) (name * name) list
  | RecordConcatInstr of (*record*) name  * (*record*) name
  | PrimitiveInstr    of (*name*) string  * (*operands*) name list
end = NameType

and NameIdentity : Hashtbl.HashedType =
struct
  type t = NameType.name

  let equal = (==)
  let hash name =
    name.NameType.n_hash
end

and Nametbl : Hashtbl.S with type key = NameType.name =
  Hashtbl.Make(NameIdentity)

and LambdaIdentity : Hashtbl.HashedType =
struct
  type t = Rt.lambda

  let equal = (==)
  let hash lambda =
    lambda.Rt.l_hash
end

and Lambdatbl : Hashtbl.S with type key = Rt.lambda =
  Hashtbl.Make(LambdaIdentity)

include NameType

exception ConvergenceFailure of Diagnostic.t list

let fail_convergence message locations =
  raise (ConvergenceFailure [Diagnostic.Error, message, locations])

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

let symtab_of_name name =
  match name with
  | { n_parent = ParentFunction {
        opcode = Function func } }
  | { n_parent = ParentBasicBlock {
      n_parent = ParentFunction {
        opcode = Function func} } }
  -> func.f_symtab
  | _
  -> assert false

let const ?loc value =
  {
    id        = "";
    ty        = Rt.type_of_value value;
    opcode    = Const value;
    location  = Option.default Location.empty loc;
    n_parent  = ParentNone;
    n_uses    = [];
    n_hash    = Hash_seed.make ();
  }

let set_id name id =
  match name with
  | { opcode = Function _; n_parent = ParentNone }
  -> name.id <- id (* Sanitized in add_func *)
  | { opcode = Function _; n_parent = ParentCapsule capsule }
  -> name.id <- Symtab.update capsule.c_symtab name.id id
  | _
  -> (let symtab = symtab_of_name name in
      name.id <- Symtab.update symtab name.id id)

let create_capsule () =
  { functions     = [];
    overloads     = Nametbl.create 10;
    lambda_cache  = Lambdatbl.create 10;
    c_symtab      = Symtab.create (); }

let iter_funcs ~f capsule =
  List.iter f capsule.functions

let find_func capsule id =
  List.find (fun funcn -> funcn.id = id) capsule.functions

let add_func capsule funcn =
  let func = func_of_name funcn in
  funcn.id <- Symtab.add capsule.c_symtab func.name;
  capsule.functions <- funcn :: capsule.functions

let remove_func capsule funcn =
  Symtab.remove capsule.c_symtab funcn.id;
  capsule.functions <- List.remove_if ((==) funcn) capsule.functions;

  (* Remove overloads where this function is either source or target. *)
  Nametbl.iter (fun source (ty, target) ->
      if funcn == source || funcn == target then
        (* It's not possible to remove one particular name-value binding
           if there are some, so remove all of them and then re-add the
           ones we keep. *)
        let lst = Nametbl.find_all capsule.overloads source in

        (* Remove all bindings. *)
        for i = 0 to (List.length lst) do
          Nametbl.remove capsule.overloads source
        done;

        (* Don't add anything if we're removing the source. *)
        if funcn != source then
          (* Re-add all bindings except the target. *)
          let lst = List.remove_if (fun (_, target') -> target' == target) lst in
          List.iter (Nametbl.add capsule.overloads source) lst)
    capsule.overloads;

  (* Remove lambda cache entries where this function is the target. *)
  Lambdatbl.iter (fun lambda target ->
      if funcn == target then
        Lambdatbl.remove capsule.lambda_cache lambda)
    capsule.lambda_cache

let create_func ?(name="") ?loc ?arg_ids ?arg_locs args_ty result_ty =
  let symtab = Symtab.create () in
  let func  = {
    name;
    arguments    = [];
    basic_blocks = [];
    f_symtab     = symtab;
    f_original   = None;
  } in
  let funcn = {
    id        = name;
    ty        = Rt.FunctionTy (args_ty, result_ty);
    opcode    = Function func;
    location  = Option.default Location.empty loc;
    n_parent  = ParentNone;
    n_uses    = [];
    n_hash    = Hash_seed.make ();
  } in
  begin
    let make_arg (id, loc) ty = {
      id        = Symtab.add symtab id;
      ty;
      opcode    = Argument;
      location  = loc;
      n_parent  = ParentFunction funcn;
      n_uses    = [];
      n_hash    = Hash_seed.make ();
    }
    in
    match arg_ids, arg_locs with
    | Some ids, Some locations
    -> func.arguments <- List.map2 make_arg (List.combine ids locations) args_ty
    | Some ids, None
    -> (let ids_locs = List.map (fun id -> id, Location.empty) ids in
        func.arguments <- List.map2 make_arg ids_locs args_ty)
    | None, None
    -> func.arguments <- List.map (make_arg ("", Location.empty)) args_ty
    | _
    -> assert false
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

let iter_args ~f funcn =
  let func = func_of_name funcn in
  List.iter f func.arguments

let create_block id =
  {
    id;
    ty        = Rt.BasicBlockTy;
    opcode    = BasicBlock { instructions = [] };
    location  = Location.empty;
    n_parent  = ParentNone;
    n_uses    = [];
    n_hash    = Hash_seed.make ();
  }

let block_parent blockn =
  match blockn.n_parent with
  | ParentFunction funcn -> funcn
  | _ -> assert false

let add_block funcn blockn =
  let func = func_of_name funcn in
  blockn.id <- Symtab.add func.f_symtab blockn.id;
  blockn.n_parent <- ParentFunction funcn;
  func.basic_blocks <- func.basic_blocks @ [blockn]

let remove_block blockn =
  let func = (func_of_name (block_parent blockn)) in
  Symtab.remove func.f_symtab blockn.id;
  func.basic_blocks <- List.remove_if ((==) blockn) func.basic_blocks

let iter_instrs ~f blockn =
  match blockn with
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
      -> (match use.n_parent with
          | ParentBasicBlock block -> Some block
          | _ -> assert false)
      | _
      -> None)
    blockn.n_uses

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
  | SelectInstr (cond, if_true, if_false)
  -> [cond; if_true; if_false]
  | ReturnInstr value
  -> [value]
  | FrameInstr parent
  -> [parent]
  | LVarLoadInstr (env, _)
  -> [env]
  | LVarStoreInstr (env, _, value)
  -> [env; value]
  | IVarLoadInstr (obj, _)
  -> [obj]
  | IVarStoreInstr (obj, _, value)
  -> [obj; value]
  | CallInstr (callee, operands)
  -> callee :: operands
  | ClosureInstr (func, env)
  -> [func; env]
  | SpecializeInstr (cls, specz)
  -> cls :: Assoc.values specz
  | TupleExtendInstr (tup, elems)
  -> tup :: elems
  | TupleConcatInstr (tup, tup')
  -> [tup; tup']
  | RecordExtendInstr (re, elems)
  -> re :: List.concat (List.map (fun (x, y) -> [x; y]) elems)
  | RecordConcatInstr (re, re')
  -> [re; re']
  | PrimitiveInstr (name, operands)
  -> operands

let instr_parent instr =
  match instr.n_parent with
  | ParentBasicBlock blockn -> blockn
  | _ -> assert false

let add_uses instr =
  List.iter (fun used ->
      assert (not (List.memq instr used.n_uses));
      used.n_uses <- instr :: used.n_uses)
    (instr_operands instr)

let remove_uses instr =
  List.iter (fun used ->
      assert (List.memq instr used.n_uses);
      used.n_uses <- List.remove_if ((==) instr) used.n_uses)
    (instr_operands instr)

let iter_uses ~f instr =
  List.iter f instr.n_uses

let create_instr ?(id="") ?loc ty opcode =
  let instr = {
    id;
    ty;
    opcode;
    location  = Option.default Location.empty loc;
    n_parent  = ParentNone;
    n_uses    = [];
    n_hash    = 0;
  }
  in
  add_uses instr;
  instr

let insert_instr ?pivot f_some f_none instr blockn =
  let block  = block_of_name blockn in
  begin
    (* Sanity checks: instr must be an instruction not
       attached to any block. *)
    assert (instr.n_parent = ParentNone);
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
            [] (List.rev block.instructions))
    | None
    -> block.instructions <- f_none block.instructions
  end;
  let symtab = symtab_of_name blockn in
  instr.id       <- Symtab.add symtab instr.id;
  instr.n_parent <- ParentBasicBlock blockn

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

let set_opcode name opcode =
  match name.opcode with
  | Argument | Function _ | BasicBlock _ | Const _
  -> assert false
  | _ (* instruction *)
  -> (remove_uses name;
      name.opcode <- opcode;
      add_uses name)

let set_ty name ty =
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
  (* Remove the name from symbol table. *)
  let symtab = symtab_of_name instr in
  Symtab.remove symtab instr.id;

  (* Remove the instruction from its parent basic block. *)
  match instr.n_parent with
  | ParentBasicBlock blockn
  -> (let block = block_of_name blockn in
      assert (List.memq instr block.instructions);
      block.instructions <- List.remove_if ((==) instr) block.instructions;
      instr.n_parent <- ParentNone)
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
  | SelectInstr _, [cond; if_true; if_false]
  -> SelectInstr (cond, if_true, if_false)
  | FrameInstr _,  [parent]
  -> FrameInstr parent
  | LVarLoadInstr (_, name), [frame]
  -> LVarLoadInstr (frame, name)
  | LVarStoreInstr (_, name, _), [frame; value]
  -> LVarStoreInstr (frame, name, value)
  | IVarLoadInstr (_, name), [obj]
  -> IVarLoadInstr (obj, name)
  | IVarStoreInstr (_, name, _), [obj; value]
  -> IVarStoreInstr (obj, name, value)
  | CallInstr (_, _), callee :: operands
  -> CallInstr (callee, operands)
  | ClosureInstr (_, _), [func; env]
  -> ClosureInstr (func, env)
  | SpecializeInstr (_, specz), cls :: specz'
  -> SpecializeInstr (cls, Assoc.update specz specz')
  | TupleExtendInstr (_, _), tup :: elems
  -> TupleExtendInstr (tup, elems)
  | TupleConcatInstr (_, _), [tup; tup']
  -> TupleConcatInstr (tup, tup')
  | RecordExtendInstr (_, _), re :: elems
  -> (let rec pack elems =
        match elems with
        | k :: v :: rest -> (k, v) :: pack rest
        | [] -> []
        | _ -> assert false
      in
      RecordExtendInstr (re, pack elems))
  | RecordConcatInstr (_, _), [re; re']
  -> RecordConcatInstr (re, re')
  | PrimitiveInstr (name, _), _
  -> PrimitiveInstr (name, operands)
  | _
  -> assert false

let set_instr_operands instr operands =
  set_opcode instr (map_instr_operands instr operands)

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
  -> erase_instr instr
  | _
  -> prepend_instr ~before:instr instr' (instr_parent instr);
     erase_instr instr

let copy_func ?(suffix="") funcn =
  let func    = func_of_name funcn in
  (* Duplicate the function. *)
  let arg_ids = List.map (fun arg -> arg.id ^ suffix) func.arguments in
  let args_ty, ret_ty = func_ty funcn in
  let funcn' = create_func ~name:func.name ~arg_ids args_ty ret_ty in
  let func'  = func_of_name funcn' in
  (* Duplicate function content while maintaining referentional
     integrity. *)
  let map    = Nametbl.create 10 in
  (* Remember the mapping between arguments. *)
  List.iter2 (Nametbl.add map) func.arguments func'.arguments;
  (* Duplicate basic blocks and remember the mapping between them. *)
  iter_blocks funcn ~f:(fun blockn ->
    let blockn' = create_block (blockn.id ^ suffix) in
    add_block funcn' blockn';
    Nametbl.add map blockn blockn');

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
      -> Nametbl.find map operand
    in
    let operands' = List.map map_operand (instr_operands instr) in
    let opcode'   = map_instr_operands instr operands' in
    set_opcode instr' opcode'
  in
  let phis = ref [] in
  iter_instrs funcn ~f:(fun instr ->
    (* Duplicate instruction. *)
    let instr'  = create_instr ~loc:instr.location ~id:(instr.id ^ suffix)
                               instr.ty InvalidInstr in
    (* Append instruction to the corresponding basic block in the
       specialized function. *)
    let blockn' =
      match instr.n_parent with
      | ParentBasicBlock blockn -> Nametbl.find map blockn
      | _ -> assert false
    in
    append_instr instr' blockn';
    (* Remember the mapping for the current instruction. *)
    Nametbl.add map instr instr';
    (* Map instruction operands. *)
    match instr.opcode with
    (* Phi instructions will be fixed up later. *)
    | PhiInstr _ -> phis := (instr, instr') :: !phis
    | _          -> map_opcode instr instr');
  (* Fix up all phis. *)
  List.iter (fun (phi, phi') -> map_opcode phi phi') !phis;
  funcn'

let specialize funcn env =
  let changed = ref false in
  (* Update type of an SSA name. *)
  let update_ty name =
    let ty, ty' = name.ty, Typing.subst env name.ty in
    if not (Rt.equal ty ty') then begin
      set_ty name ty';
      changed := true
    end
  in
  (* Update operands of an instruction. *)
  let update_operands instr =
    let op_changed = ref false in
    let operands   = instr_operands instr in
    let operands'  = List.map (fun operand ->
        match operand with
        | { opcode = Const value }
        -> (let value' = Typing.subst env value in
            if not (Rt.equal value value') then
              op_changed := true;
              const value')
        | _
        -> operand)
      operands
    in
    if !op_changed then
      changed := true;
      set_instr_operands instr operands'
  in
  (* Specialize function type. *)
  update_ty funcn;
  (* Specialize instruction types and constant arguments. *)
  iter_instrs (fun instr ->
      update_ty instr;
      update_operands instr)
    funcn;
  !changed

let iter_overloads ~f capsule =
  Nametbl.iter (fun funcn (ty, funcn') -> f funcn ty funcn') capsule.overloads

let find_overload ~f capsule funcn =
  let overloads = Nametbl.find_all capsule.overloads funcn in
  (* Assign a specifity to all possible overloads. *)
  let overloads =
    List.filter_map (fun (ty, overload) ->
        Option.map (fun specifity ->
            overload, specifity)
          (f ty overload))
      overloads
  in
  (* Sort overloads by specifity; from high to low. *)
  let overloads =
    List.sort ~cmp:(fun a b -> compare (fst b) (fst a)) overloads
  in
  (* Pick the most specific overload or raise. *)
  match overloads with
  | (overload, _) :: _ -> overload
  | [] -> raise Not_found

let add_overload capsule funcn ty funcn' =
  let _func = func_of_name funcn
  and func' = func_of_name funcn' in
  func'.f_original <- Some funcn;
  Nametbl.add capsule.overloads funcn (ty, funcn')

let iter_lambdas ~f capsule =
  Lambdatbl.iter f capsule.lambda_cache

let lookup_lambda capsule lambda =
  try  Some (Lambdatbl.find capsule.lambda_cache lambda)
  with Not_found -> None

let add_lambda capsule lambda funcn =
  ignore (func_of_name funcn);
  Lambdatbl.add capsule.lambda_cache lambda funcn
