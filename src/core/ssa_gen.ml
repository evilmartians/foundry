open Unicode.Std
open Big_int
open ExtList

type ssa_conv_kind =
| ConvObject
| ConvInitializer
| ConvValue
| ConvValueInitializer

type ssa_conv_state = {
          capsule   : Ssa.capsule;

          funcn     : Ssa.name;
          kind      : ssa_conv_kind;

  (* Local environment state. Immutable upvalues known
     at conversion time are hoisted out of the environments
     and are treated exactly same as literals. *)
          frame     : Ssa.name;
          frame_ty  : Rt.local_env_ty;
          depth     : int;
          local_env : Rt.local_env;

  mutable const_env : Rt.const_env;
          type_env  : Rt.type_env;

  (* Argument parser state. *)
          args      : Ssa.name;
          kwargs    : Ssa.name;
  mutable arg_idx   : int;

  (* Functional update state. *)
  mutable update    : Ssa.name option;
}

let tvar () =
  Rt.Tvar (Rt.new_tvar ())

let append ?(ty=Rt.NilTy) ~opcode blockn =
  let instr = Ssa.create_instr ty opcode in
  Ssa.append_instr instr blockn;
  instr

let load ~state entry name =
  let append_load ty =
    let instr = append entry ~ty ~opcode:(Ssa.LVarLoadInstr (state.frame, name)) in
    Ssa.set_id instr ("lvar." ^ name);
    instr
  in
  let rec lookup_sta env =
    match Table.get env.Rt.e_bindings name with
    | Some ({ Rt.b_kind = Syntax.LVarImmutable } as binding)
    -> Ssa.const binding.Rt.b_value
    | Some binding
    -> append_load (Rt.type_of_value binding.Rt.b_value)
    | None
    -> (match env.Rt.e_parent with
        | Some parent -> lookup_sta parent
        | None        -> assert false)
  in
  let rec lookup_dyn env_ty depth =
    if depth > 0 then
      match Table.get env_ty.Rt.e_ty_bindings name with
      | Some binding
      -> append_load binding.Rt.b_ty
      | None
      -> (match env_ty.Rt.e_ty_parent with
          | Some parent_ty -> lookup_dyn parent_ty (depth - 1)
          | None           -> assert false)
    else
      lookup_sta state.local_env
  in
  lookup_dyn state.frame_ty state.depth

let store ~state entry name value =
  ignore (append entry
               ~opcode:(Ssa.LVarStoreInstr (state.frame, name, value)));
  value

let send_args ~state entry args =
  append entry ~ty:(tvar ())
               ~opcode:(Ssa.TupleExtendInstr (Ssa.const (Rt.Tuple []), args)),
  Ssa.const (Rt.Record Assoc.empty)

let send ~state entry receiver selector args kwargs =
  let callee =
    append entry ~ty:(tvar ())
                 ~opcode:(Ssa.ResolveInstr (receiver,
                    (Ssa.const (Rt.Symbol selector))))
  in
  append entry ~ty:(tvar ())
               ~opcode:(Ssa.CallInstr (callee, [args; kwargs]))

let rec ssa_of_expr ~entry ~state ~expr =
  let load      = load  ~state in
  let store     = store ~state in
  let send_args = send_args ~state in
  let send      = send  ~state in
  match expr with
  (* Literals. *)
  | Syntax.Nil (_)
  -> entry, Ssa.const (Rt.Nil)
  | Syntax.Truth (_)
  -> entry, Ssa.const (Rt.Truth)
  | Syntax.Lies (_)
  -> entry, Ssa.const (Rt.Lies)
  | Syntax.Symbol (_, value)
  -> entry, Ssa.const (Rt.Symbol value)
  | Syntax.Integer (_, value)
  -> entry, Ssa.const (Rt.Integer value)
  | Syntax.Unsigned (_, width, value)
  -> entry, Ssa.const (Rt.Unsigned (width, value))
  | Syntax.Signed (_, width, value)
  -> entry, Ssa.const (Rt.Signed (width, value))

  | Syntax.Tuple (_, elems)
  -> (let entry, interp =
        List.fold_left (fun (entry, interp) elem ->
            match elem with
            | Syntax.TupleElem (_, expr)
            -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
                entry, Ssa_interp.append interp (Ssa_interp.Elem value))
            | Syntax.TupleSplice (_, expr)
            -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
                entry, Ssa_interp.append interp (Ssa_interp.Splice value)))
          (entry, Ssa_interp.empty) elems
      in
      entry, Ssa_interp.tup_apply interp entry)

  | Syntax.Record (_, elems)
  -> (let entry, interp =
        List.fold_left (fun (entry, interp) elem ->
            match elem with
            | Syntax.RecordElem (_, name, expr)
            -> (let name = Ssa.const (Rt.Symbol name) in
                let entry, value = ssa_of_expr ~entry ~state ~expr in
                entry, Ssa_interp.append interp (Ssa_interp.Elem (name, value)))
            | Syntax.RecordPair (_, name_expr, value_expr)
            -> (let entry, name  = ssa_of_expr ~entry ~state ~expr:name_expr  in
                let entry, value = ssa_of_expr ~entry ~state ~expr:value_expr in
                entry, Ssa_interp.append interp (Ssa_interp.Elem (name, value)))
            | Syntax.RecordSplice (_, expr)
            -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
                entry, Ssa_interp.append interp (Ssa_interp.Splice value)))
          (entry, Ssa_interp.empty) elems
      in
      entry, Ssa_interp.rec_apply interp entry)

  (* Code block. *)
  | Syntax.Begin (_, exprs)
  -> (let entry, names = ssa_of_exprs ~entry ~state ~exprs in
      entry, List.hd names)

  (* Constants. *)
  | Syntax.Const (_, name)
  -> entry, Ssa.const (Rt.cenv_lookup state.const_env name)

  (* Types. *)
  | Syntax.TVar (_, name)
  -> entry, Ssa.const (Rt.Tvar (Rt.tenv_resolve state.type_env name))
  | Syntax.Type (_, expr)
  -> ssa_of_type ~entry ~state ~expr

  (* Variable access and assignment. *)
  | Syntax.Self (_)
  -> entry, load entry "self"
  | Syntax.Var (_, name)
  -> entry, load entry name
  | Syntax.IVar (_, name)
  -> (let self = load entry "self" in
      entry, append entry ~ty:(tvar ())
                          ~opcode:(Ssa.IVarLoadInstr (self, name)))

  | Syntax.Assign (_, Syntax.Var (_, name), expr)
  -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
      entry, store entry name value)

  | Syntax.Assign (_, Syntax.IVar (_, name), expr)
    when state.update <> None
  -> (match state.update with
      | Some self
      -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
          let new_self = append entry ~ty:(self.Ssa.ty)
                                      ~opcode:(Ssa.IVarStoreInstr (self, name, value)) in
          state.update <- Some new_self;
          entry, value)
      | _
      -> assert false)

  | Syntax.Assign (_, Syntax.IVar (_, name), expr)
  -> (let self = load entry "self" in
      let entry, value = ssa_of_expr ~entry ~state ~expr in
      match state.kind with
      | ConvObject | ConvInitializer
      -> (ignore (append entry ~opcode:(Ssa.IVarStoreInstr (self, name, value)));
          entry, value)
      | ConvValueInitializer
      -> (let new_self = append entry ~ty:(self.Ssa.ty)
                                      ~opcode:(Ssa.IVarStoreInstr (self, name, value)) in
          ignore (store entry "self" new_self);
          entry, value)
      | ConvValue
      -> failwith "ivar assignment in value non-initializer")

  | Syntax.Assign (_, Syntax.Send (_, receiver, selector, operands), value)
  -> (let entry, receiver = ssa_of_expr ~entry ~state ~expr:receiver in
      let entry, value    = ssa_of_expr ~entry ~state ~expr:value    in
      let args,  kwargs   = send_args entry [receiver; value] in
      ignore (send entry receiver (selector ^ "=") args kwargs);
      entry, value)

  | Syntax.OpAssign (_, Syntax.Var (_, name), selector, expr)
  -> (let entry, arg   = ssa_of_expr ~entry ~state ~expr in
      let value        = load entry name in
      let args, kwargs = send_args entry [value; arg] in
      let value'       = send entry value selector args kwargs in
      entry, store entry name value')

  | Syntax.OpAssign (_, Syntax.Send (_, receiver, selector, operands), operator, expr)
  ->  (* Fetch the base expression. *)
     (let entry, receiver = ssa_of_expr ~entry ~state ~expr:receiver in
      (* Fetch the old value. *)
      let args,  kwargs   = send_args entry [receiver] in
      let value           = send entry receiver selector args kwargs in
      (* Fetch the argument. *)
      let entry, arg      = ssa_of_expr ~entry ~state ~expr in
      (* Perform the operation. *)
      let args,  kwargs   = send_args entry [value; arg] in
      let value'          = send entry value operator args kwargs in
      (* Write back the new value. *)
      let args,  kwargs   = send_args entry [receiver; value'] in
      ignore (send entry receiver (selector ^ "=") args kwargs);
      entry, value')

  | Syntax.Let (_, pattern, _ty, expr)
  -> ssa_of_pattern ~entry ~state ~pattern ~expr

  (* Method calls. *)
  | Syntax.Send (_, receiver, selector, actual_args)
  -> (let entry, receiver     = ssa_of_expr ~entry ~state ~expr:receiver in
      let entry, args, kwargs = ssa_of_actual_args ~entry ~state ~receiver ~actual_args in
      entry, send entry receiver selector args kwargs)

  (* Control flow. *)
  | Syntax.If (_, cond, true_exprs, false_expr)
  -> (let head, cond = ssa_of_expr ~entry ~state ~expr:cond in
      let true_pred = Ssa.create_block state.funcn in
      let true_entry, true_value =
        ssa_of_seq ~entry:true_pred ~state ~exprs:true_exprs in
      let false_tup =
        Option.map (fun expr ->
            let false_entry = Ssa.create_block state.funcn in
            false_entry, ssa_of_expr ~entry:false_entry ~state ~expr)
          false_expr
      in
      let tail = Ssa.create_block state.funcn in
      let false_pred, false_entry, false_value =
        match false_tup with
        | Some (pred, (entry, value))
        -> pred, entry, value
        | None
        -> tail, head, Ssa.const Rt.Nil
      in
      ignore (append head ~opcode:(Ssa.JumpIfInstr (cond, true_pred, false_pred)));
      ignore (append true_entry ~opcode:(Ssa.JumpInstr tail));
      if false_entry != head then
        ignore (append false_entry ~opcode:(Ssa.JumpInstr tail));
      tail, append tail ~ty:(tvar ())
                ~opcode:(Ssa.PhiInstr [true_entry,  true_value;
                                       false_entry, false_value]))
  | Syntax.While (_, cond, exprs)
  | Syntax.Until (_, cond, exprs)
  -> (let head = Ssa.create_block state.funcn in
      ignore (append entry ~opcode:(Ssa.JumpInstr head));
      let head, cond = ssa_of_expr ~entry:head ~state ~expr:cond in
      let pred = Ssa.create_block state.funcn in
      let body, _ = ssa_of_seq ~entry:pred ~state ~exprs in
      ignore (append body ~opcode:(Ssa.JumpInstr head));
      let tail = Ssa.create_block state.funcn in
      let if_true, if_false =
        match expr with
        | Syntax.While _ -> pred, tail
        | Syntax.Until _ -> tail, pred
        | _ -> assert false
      in
      ignore (append head ~opcode:(Ssa.JumpIfInstr (cond, if_true, if_false)));
      tail, Ssa.const Rt.Nil)

  | Syntax.Or (_, lhs, rhs)
  | Syntax.And (_, lhs, rhs)
  -> (let head, lhs_value = ssa_of_expr ~entry ~state ~expr:lhs in
      let       rhs_pred  = Ssa.create_block state.funcn in
      let body, rhs_value = ssa_of_expr ~entry:rhs_pred ~state ~expr:rhs in
      let tail            = Ssa.create_block state.funcn in
      ignore (append body ~opcode:(Ssa.JumpInstr tail));
      let if_true, if_false =
        match expr with
        | Syntax.Or  _ -> tail, rhs_pred
        | Syntax.And _ -> rhs_pred, tail
        | _ -> assert false
      in
      ignore (append head ~opcode:(Ssa.JumpIfInstr (lhs_value, if_true, if_false)));
      tail, append tail ~ty:(tvar ())
                ~opcode:(Ssa.PhiInstr [head, lhs_value;
                                       body, rhs_value]))

  | Syntax.Not (_, expr)
  -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
      entry, append entry ~ty:Rt.BooleanTy
                          ~opcode:(Ssa.PrimitiveInstr ("bool_neg", [value])))

  (* Miscellanea. *)
  | Syntax.Lambda (_, formal_args, ty, expr) (* TODO lam_ty *)
  -> ssa_of_lambda_expr ~entry ~state ~formal_args ~expr

  | Syntax.Update (_, exprs)
  ->  (* Initiate functional update mode by switching the function
         of @var assignment syntax. *)
     (assert (state.update = None);
      state.update <- Some (load entry "self");
      (* Translate the body with modified @vars. *)
      let entry, _ = ssa_of_exprs ~entry ~state ~exprs in
      (* Extract the update result and restore @var mode. *)
      match state.update with
      | Some value
      -> (state.update <- None;
          entry, value)
      | _
      -> assert false)

  | Syntax.InvokePrimitive (_, name, operands)
  -> (let entry, operands = ssa_of_exprs ~entry ~state ~exprs:operands in
      entry, append entry
              ~ty:(tvar ())
              ~opcode:(Ssa.PrimitiveInstr (name, List.rev operands)))

  | _
  -> failwith ("ssa_of_expr: " ^
        (Unicode.assert_utf8s
          (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr))))

and ssa_of_exprs ~entry ~state ~exprs =
  List.fold_left
    (fun (entry, names) expr ->
      let entry, name = ssa_of_expr ~entry ~state ~expr in
        entry, name :: names)
    (entry, []) exprs

and ssa_of_seq ~entry ~state ~exprs =
  let entry, exprs = ssa_of_exprs ~entry ~state ~exprs in
  match exprs with
  | []
  | { Ssa.ty = Rt.NilTy } :: _
  -> entry, Ssa.const Rt.Nil
  | expr :: _
  -> entry, expr

and ssa_of_type ~entry ~state ~expr =
  match expr with
  | Syntax.TypeConstr (_, name, args)
  -> (let ty = Rt.cenv_lookup state.const_env name in
      let klass, specz = Typing.unfold_equiv ty in
      let specz = Assoc.map specz ~f:(fun _ -> Ssa.const) in
      let rec combine entry params specz args =
        match args with
        | Syntax.TypeArg (_, ty) :: args
        -> (let (name, _), params = Assoc.pluck params in
            let entry, ty = ssa_of_type ~entry ~state ~expr:ty in
            let specz = Assoc.add specz name ty in
            combine entry params specz args)
        | Syntax.TypeKwArg (_, name, ty) :: args
        -> (let entry, ty = ssa_of_type ~entry ~state ~expr:ty in
            let specz = Assoc.add specz name ty in
            combine entry params specz args)
        | []
        -> specz
      in
      let specz = combine entry klass.Rt.k_parameters specz args in
      entry, append entry ~ty:(tvar ())
                          ~opcode:(Ssa.SpecializeInstr (Ssa.const ty, specz)))
  | Syntax.TypeSplice (_, expr)
  -> ssa_of_expr ~entry ~state ~expr
  | _
  -> assert false

and ssa_of_pattern ~entry ~state ~pattern ~expr =
  match pattern with
  | Syntax.PatVariable (_, (kind, name))
  -> (let ty = tvar () in
      Table.set state.frame_ty.Rt.e_ty_bindings name {
        Rt.b_ty_location = Location.empty;
        Rt.b_ty_kind     = kind;
        Rt.b_ty    = ty;
      };
      let entry, expr = ssa_of_expr ~entry ~state ~expr in
      ignore (append entry ~opcode:(Ssa.LVarStoreInstr (state.frame, name, expr)));
      entry, expr)
  | _
  -> assert false

and ssa_of_actual_args ~entry ~state ~receiver ~actual_args =
  let args = Ssa_interp.append Ssa_interp.empty (Ssa_interp.Elem receiver) in
  let entry, args, kwargs =
    List.fold_left (fun (entry, args, kwargs) actual_arg ->
        match actual_arg with
        | Syntax.ActualArg (_, expr)
        -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
            entry, Ssa_interp.append args (Ssa_interp.Elem value), kwargs)
        | Syntax.ActualSplice (_, expr)
        -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
            entry, Ssa_interp.append args (Ssa_interp.Splice value), kwargs)
        | Syntax.ActualKwArg (_, name, expr)
        -> (let name = Ssa.const (Rt.Symbol name) in
            let entry, value = ssa_of_expr ~entry ~state ~expr in
            entry, args, Ssa_interp.append kwargs (Ssa_interp.Elem (name, value)))
        | Syntax.ActualKwPair (_, name_expr, value_expr)
        -> (let entry, name  = ssa_of_expr ~entry ~state ~expr:name_expr  in
            let entry, value = ssa_of_expr ~entry ~state ~expr:value_expr in
            entry, args, Ssa_interp.append kwargs (Ssa_interp.Elem (name, value)))
        | Syntax.ActualKwSplice (_, expr)
        -> (let entry, value = ssa_of_expr ~entry ~state ~expr in
            entry, args, Ssa_interp.append kwargs (Ssa_interp.Splice value)))
      (entry, args, Ssa_interp.empty) actual_args
  in
  entry, (Ssa_interp.tup_apply args entry), (Ssa_interp.rec_apply kwargs entry)

and ssa_of_formal_args ~entry ~state ~formal_args =
  (* Internally args and kwargs are treated disjointly. Split syntactic
     args to better match this representation. *)
  let args, kwargs =
    List.partition (fun arg ->
        match arg with
        | Syntax.FormalSelf _
        | Syntax.FormalArg _
        | Syntax.FormalOptArg _
        | Syntax.FormalRest _
        -> true
        | Syntax.FormalKwArg _
        | Syntax.FormalKwOptArg _
        | Syntax.FormalKwRest _
        -> false)
      formal_args
  in
  (* Find out how args to skip from the end for *rest. *)
  let rest_index =
    try
      fst (List.findi (fun _ arg ->
              match arg with
              | Syntax.FormalRest _ -> true
              | _ -> false)
            args)
    with Not_found ->
      (* The value of rest_index is never used if there is no
         FormalRest in the args. 0 works. *)
      0
  in
  let post_count = (List.length args) - rest_index - 1
  in
  (* Make sure the order of evaluation matches lexical order of
     arguments. *)
  List.fold_left (fun entry formal_arg ->
      (* All bindings initially have a fresh type variable as
         their type. *)
      let ty = tvar () in
      (* Helper functions to assemble type-level calculations. *)
      let int n = Ssa.const (Rt.Integer (big_int_of_int n))
      and sym s = Ssa.const (Rt.Symbol s) in
      let right_idx idx =
        let args_len =
          append entry ~ty:(tvar ())
                       ~opcode:(Ssa.PrimitiveInstr ("tup_length",
                                    [state.args])) in
        append entry ~ty:(tvar ())
                     ~opcode:(Ssa.PrimitiveInstr ("int_sub",
                                  [args_len; idx]))
      in
      let assign kind name value =
        Table.set state.frame_ty.Rt.e_ty_bindings name {
          Rt.b_ty_location = Location.empty;
          Rt.b_ty_kind     = kind;
          Rt.b_ty          = ty;
        };
        ignore (append entry ~opcode:(Ssa.LVarStoreInstr (state.frame, name, value)));
        entry
      in
      match formal_arg with
      | Syntax.FormalSelf (_)
      -> (let arg =
            append entry ~ty ~opcode:(Ssa.PrimitiveInstr ("tup_lookup",
                                      [state.args; int 0])) in
          state.arg_idx <- 1;
          let lvar_kind =
            match state.kind with
            | ConvValueInitializer -> Syntax.LVarMutable
            | _ -> Syntax.LVarImmutable
          in
          assign lvar_kind "self" arg)
      | Syntax.FormalArg (_, (kind, name))
      -> (let arg_idx =
            if state.arg_idx >= 0 then begin
              let idx_name = int state.arg_idx in
              state.arg_idx <- state.arg_idx + 1;
              idx_name
            end else begin
              let idx_name = right_idx (int (-state.arg_idx)) in
              state.arg_idx <- state.arg_idx - 1;
              idx_name
            end
          in
          let arg =
            append entry ~ty ~opcode:(Ssa.PrimitiveInstr ("tup_lookup",
                                      [state.args; arg_idx])) in
          assign kind name arg)
      | Syntax.FormalRest (_, (kind, name))
      -> (let last_idx = right_idx (int post_count) in
          let args =
            append entry ~ty ~opcode:(Ssa.PrimitiveInstr ("tup_slice",
                                      [state.args; int state.arg_idx; last_idx])) in
          assign kind name args)
      | Syntax.FormalKwArg (_, (kind, name))
      -> (let arg =
            append entry ~ty ~opcode:(Ssa.PrimitiveInstr ("rec_lookup",
                                      [state.kwargs; sym name]))
          in
          assign kind name arg)
      | Syntax.FormalKwRest (_, (kind, name))
      -> (* TODO stub *)
          assign kind name state.kwargs
      | _
      -> assert false)
    entry formal_args

and ssa_of_lambda_expr ~entry ~state ~formal_args ~expr =
  let funcn =
    Ssa.create_func
      ~arg_ids:["env"; "args"; "kwargs"]
      [Rt.EnvironmentTy state.frame_ty;
       tvar (); tvar ()] (tvar ())
  in
  let func = Ssa.func_of_name funcn in

  (* Register the created artifact. *)
  Ssa.add_func state.capsule funcn;

  (* Extract arguments as SSA names. *)
  let env, args, kwargs =
    match func.Ssa.arguments with
    | [env; args; kwargs] -> env, args, kwargs
    | _ -> assert false
  in

  (* Create entry block. *)
  let lam_entry = Ssa.create_block ~id:"entry" funcn in
    (* Define a stack frame. Its type will be mutated while the function
       is converted. *)
    let frame_ty = {
      Rt.e_ty_parent   = Some (state.frame_ty);
      Rt.e_ty_bindings = Table.create [];
    }
    in
    let frame =
      append lam_entry ~ty:(Rt.EnvironmentTy frame_ty)
                   ~opcode:(Ssa.FrameInstr env)
    in

    (* Perform SSA conversion. *)
    let lam_state = { state with
                      funcn; frame; frame_ty;
                      type_env = Table.copy state.type_env;
                      depth    = state.depth + 1;
                      args; arg_idx = 0; kwargs } in
    let lam_entry = ssa_of_formal_args ~entry:lam_entry ~state:lam_state ~formal_args in
    let lam_entry, lam_value = ssa_of_expr ~entry:lam_entry ~state:lam_state ~expr in
    ignore (append lam_entry ~ty:Rt.NilTy ~opcode:(Ssa.ReturnInstr lam_value));

  (* Refer to the closure in the parent function. *)
  match Ssa.func_ty funcn with
  | [_; args_ty; kwargs_ty], ret_ty
  -> (entry, append entry ~ty:(Rt.ClosureTy ([args_ty; kwargs_ty], ret_ty))
                          ~opcode:(Ssa.ClosureInstr (funcn, state.frame)))
  | _
  -> assert false

let name_of_lambda klass selector lambda capsule =
  let id = klass.Rt.k_name ^ ":" ^ selector in

  (* Create the function with the signature corresponding to that
     of lambda. Usually it would be (\x, \y) -> \z. *)
  let funcn =
    let ty = lambda.Rt.l_ty in
    Ssa.create_func ~id
      ~arg_ids:["args"; "kwargs"]
      [ty.Rt.l_ty_args; ty.Rt.l_ty_kwargs]
      ty.Rt.l_ty_result
  in
  let func = Ssa.func_of_name funcn in

  (* Register the created artifacts. *)
  Ssa.add_func   capsule funcn;
  Ssa.add_lambda capsule lambda funcn;

  (* Extract arguments as SSA names. *)
  let args, kwargs =
    match func.Ssa.arguments with
    | [args; kwargs] -> args, kwargs
    | _ -> assert false
  in

  (* Create entry block. *)
  let entry = Ssa.create_block ~id:"entry" funcn in
    (* Define a stack frame. Its type will be mutated while the function
       is converted. *)
    let frame_ty = {
      Rt.e_ty_parent   = Some (Rt.type_of_environment ~imm:false lambda.Rt.l_local_env);
      Rt.e_ty_bindings = Table.create [];
    }
    in
    let frame =
      append entry ~ty:(Rt.EnvironmentTy frame_ty)
                   ~opcode:(Ssa.FrameInstr (Ssa.const
                             (Rt.Environment lambda.Rt.l_local_env)))
    in

    (* Set special conversion parameters for this function. *)
    let kind =
      match klass.Rt.k_is_value, (selector : string :> latin1s) with
      | false, "initialize" -> ConvInitializer
      | false, _            -> ConvObject
      | true,  "initialize" -> ConvValueInitializer
      | true,  _            -> ConvValue
    in

    (* Perform SSA conversion. *)
    let state = { capsule; kind; funcn; frame; frame_ty;
                  depth     = 1;
                  local_env = lambda.Rt.l_local_env;
                  type_env  = lambda.Rt.l_type_env;
                  const_env = lambda.Rt.l_const_env;
                  args; arg_idx = 0; kwargs;
                  update    = None; } in
    let entry = ssa_of_formal_args ~entry ~state ~formal_args:lambda.Rt.l_args in
    let entry, value = ssa_of_seq ~entry ~state ~exprs:lambda.Rt.l_body in

    begin match kind with
    | ConvInitializer | ConvValueInitializer
    -> (let self = load ~state entry "self" in
        ignore (append entry ~opcode:(Ssa.ReturnInstr self)))
    | _
    -> ignore (append entry ~opcode:(Ssa.ReturnInstr value))
    end;

    funcn
