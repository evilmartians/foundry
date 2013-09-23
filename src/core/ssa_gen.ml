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

  (* Functional update state. *)
  mutable update    : Ssa.name option;
}

let append ?(ty=Rt.NilTy) ~opcode blockn =
  let instr = Ssa.create_instr ty opcode in
  Ssa.append_instr instr blockn;
  instr

let append_int entry value =
  let value = Ssa.const value
  and ty    = Rt.Class ((!Rt.roots).Rt.kFixed,
                        Assoc.sorted ["width",  Rt.tvar_as_ty ();
                                      "signed", Rt.tvar_as_ty ()]) in
  append entry ~ty ~opcode:(Ssa.PrimitiveInstr ("int_coerce", [value]))

let append_const entry value =
  match value with
  | Rt.Integer _ -> append_int entry value
  | _            -> Ssa.const value

let load ~state entry name =
  let append_load ty =
    let instr = append entry ~ty ~opcode:(Ssa.LVarLoadInstr (state.frame, name)) in
    Ssa.set_id instr ("lvar." ^ name);
    instr
  in
  let rec lookup_sta env =
    match Table.get env.Rt.e_bindings name with
    | Some ({ Rt.b_kind = Syntax.LVarImmutable } as binding)
    -> append_const entry binding.Rt.b_value
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
  append entry ~ty:(Rt.tvar_as_ty ())
               ~opcode:(Ssa.TupleExtendInstr (Ssa.const (Rt.Tuple []), args)),
  Ssa.const (Rt.Record Assoc.empty)

let send ~state entry receiver selector args kwargs =
  append entry ~ty:(Rt.tvar_as_ty ())
               ~opcode:(Ssa.PrimitiveInstr ("obj_send", [
                          receiver;
                          Ssa.const (Rt.Symbol selector);
                          args; kwargs
                        ]))

let rec ssa_of_assign ~state ~entry ~lhs ~value =
  match lhs with
  | Syntax.Var (_, name)
  -> entry, store ~state entry name value

  | Syntax.IVar (_, name)
  -> (match state.update with
      | Some self
      -> (let new_self = append entry ~ty:(self.Ssa.ty)
                                      ~opcode:(Ssa.IVarStoreInstr (self, name, value)) in
          state.update <- Some new_self;
          entry, value)
      | _
      -> (let self = load ~state entry "self" in
          match state.kind with
          | ConvObject | ConvInitializer
          -> (ignore (append entry ~opcode:(Ssa.IVarStoreInstr (self, name, value)));
              entry, value)
          | ConvValueInitializer
          -> (let new_self = append entry ~ty:(self.Ssa.ty)
                                          ~opcode:(Ssa.IVarStoreInstr (self, name, value)) in
              ignore (store ~state entry "self" new_self);
              entry, value)
          | ConvValue
          -> failwith "ivar assignment in value non-initializer"))

  | _
  -> assert false

and ssa_of_expr ~state ~entry ~expr =
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
  -> entry, append_int entry (Rt.Integer value)
  | Syntax.Unsigned (_, width, value)
  -> entry, Ssa.const (Rt.Unsigned (width, value))
  | Syntax.Signed (_, width, value)
  -> entry, Ssa.const (Rt.Signed (width, value))

  | Syntax.Tuple (_, elems)
  -> (let entry, interp =
        List.fold_left (fun (entry, interp) elem ->
            match elem with
            | Syntax.TupleElem (_, expr)
            -> (let entry, value = ssa_of_expr ~state ~entry ~expr in
                entry, Ssa_interp.append interp (Ssa_interp.Elem value))
            | Syntax.TupleSplice (_, expr)
            -> (let entry, value = ssa_of_expr ~state ~entry ~expr in
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
                let entry, value = ssa_of_expr ~state ~entry ~expr in
                entry, Ssa_interp.append interp (Ssa_interp.Elem (name, value)))
            | Syntax.RecordPair (_, name_expr, value_expr)
            -> (let entry, name  = ssa_of_expr ~state ~entry ~expr:name_expr  in
                let entry, value = ssa_of_expr ~state ~entry ~expr:value_expr in
                entry, Ssa_interp.append interp (Ssa_interp.Elem (name, value)))
            | Syntax.RecordSplice (_, expr)
            -> (let entry, value = ssa_of_expr ~state ~entry ~expr in
                entry, Ssa_interp.append interp (Ssa_interp.Splice value)))
          (entry, Ssa_interp.empty) elems
      in
      entry, Ssa_interp.rec_apply interp entry)

  (* Code block. *)
  | Syntax.Begin (_, exprs)
  -> (let entry, names = ssa_of_exprs ~state ~entry ~exprs in
      entry, List.hd names)

  (* Constants. *)
  | Syntax.Const (_, name)
  -> entry, append_const entry (Rt.cenv_lookup state.const_env name)

  (* Types. *)
  | Syntax.TVar (_, name)
  -> entry, Ssa.const (Rt.Tvar (Rt.tenv_resolve state.type_env name))
  | Syntax.Type (_, expr)
  -> ssa_of_type ~state ~entry ~expr

  (* Variable access. *)
  | Syntax.Self (_)
  -> entry, load entry "self"
  | Syntax.Var (_, name)
  -> entry, load entry name
  | Syntax.IVar (_, name)
  -> (let self = load entry "self" in
      entry, append entry ~ty:(Rt.tvar_as_ty ())
                          ~opcode:(Ssa.IVarLoadInstr (self, name)))

  (* Variable binding. *)
  | Syntax.Let (_, pattern, ty, expr) (* TODO ty *)
  -> ssa_of_pattern ~state ~entry ~pattern ~expr

  (* Method calls and mutation. *)
  | Syntax.Send (_, receiver, selector, actual_args)
  -> (let entry, receiver     = ssa_of_expr ~state ~entry ~expr:receiver in
      let entry, args, kwargs = ssa_of_actual_args ~state ~entry ~receiver ~actual_args in
      entry, send entry receiver selector args kwargs)

  | Syntax.Super (_, actual_args)
  -> (let self  = load entry "self" in
      let entry, args, kwargs = ssa_of_actual_args ~state ~entry ~receiver:self ~actual_args in
      let value = append entry ~ty:(Rt.tvar_as_ty ())
                               ~opcode:(Ssa.PrimitiveInstr ("obj_super", [self; args; kwargs])) in
      match state.kind with
      | ConvValueInitializer
      -> entry, store entry "self" value
      | _
      -> entry, value)

  | Syntax.Assign (_, Syntax.Send (_, receiver, selector, actual_args), arg_expr)
  ->  (* Fetch receiver and base args. *)
     (let entry, receiver     = ssa_of_expr ~state ~entry ~expr:receiver in
      let entry, args, kwargs = ssa_of_actual_args ~state ~entry ~receiver ~actual_args in
      (* Compute the assigned value. *)
      let entry, value        = ssa_of_expr ~state ~entry ~expr:arg_expr in
      (* Append the value to the positional args. *)
      let args = append entry ~ty:(Rt.tvar_as_ty ())
                              ~opcode:(Ssa.TupleExtendInstr (args, [value])) in
      (* Perform the assignment. *)
      ignore (send entry receiver (selector ^ "=") args kwargs);
      entry, value)

  | Syntax.Assign (_, lhs, expr)
  -> (let entry, value = ssa_of_expr ~state ~entry ~expr in
      ssa_of_assign ~state ~entry ~lhs ~value)

  | Syntax.OpAssign (_, Syntax.Send (_, receiver, selector, actual_args), operator, arg_expr)
  ->  (* Fetch receiver and base args. *)
     (let entry, receiver     = ssa_of_expr ~state ~entry ~expr:receiver in
      let entry, args, kwargs = ssa_of_actual_args ~state ~entry ~receiver ~actual_args in
      (* Fetch the old value. *)
      let value               = send entry receiver selector args kwargs in
      (* Compute the argument. *)
      let entry, arg          = ssa_of_expr ~state ~entry ~expr:arg_expr in
      (* Perform the operation. *)
      let op_args, op_kwargs  = send_args entry [value; arg] in
      let value'              = send entry value operator op_args op_kwargs in
      (* Write back the new value. *)
      let args = append entry ~ty:(Rt.tvar_as_ty ())
                              ~opcode:(Ssa.TupleExtendInstr (args, [value'])) in
      ignore (send entry receiver (selector ^ "=") args kwargs);
      entry, value')

  | Syntax.OpAssign (_, lhs, selector, expr)
  ->  (* Load current value. *)
     (let entry, value = ssa_of_expr ~state ~entry ~expr:lhs in
      (* Compute new value. *)
      let entry, arg   = ssa_of_expr ~state ~entry ~expr in
      let args, kwargs = send_args entry [value; arg] in
      let value'       = send entry value selector args kwargs in
      (* Write it back. *)
      ssa_of_assign ~state ~entry ~lhs ~value:value')

  (* Control flow. *)
  | Syntax.If (_, cond, true_exprs, false_expr)
  -> (let head, cond = ssa_of_expr ~state ~entry ~expr:cond in
      let true_entry = Ssa.create_block state.funcn in
      let true_exit, true_value =
        ssa_of_seq ~entry:true_entry ~state ~exprs:true_exprs in
      let false_tup =
        Option.map (fun expr ->
            let false_entry = Ssa.create_block state.funcn in
            false_entry, ssa_of_expr ~entry:false_entry ~state ~expr)
          false_expr
      in
      let tail = Ssa.create_block state.funcn in
      let false_entry, false_exit, false_value =
        match false_tup with
        | Some (entry, (exit, value))
        -> entry, exit, value
        | None
        -> tail, head, Ssa.const Rt.Nil
      in
      ignore (append head ~opcode:(Ssa.JumpIfInstr (cond, true_entry, false_entry)));
      ignore (append true_exit ~opcode:(Ssa.JumpInstr tail));
      if false_exit != head then
        ignore (append false_exit ~opcode:(Ssa.JumpInstr tail));
      tail, append tail ~ty:(Rt.tvar_as_ty ())
                        ~opcode:(Ssa.PhiInstr [true_exit,  true_value;
                                               false_exit, false_value]))

  | Syntax.Unless (_, cond, false_exprs)
  -> (let head, cond  = ssa_of_expr ~state ~entry ~expr:cond
      and false_entry = Ssa.create_block state.funcn in
      let false_exit, false_value =
        ssa_of_seq ~entry:false_entry ~state ~exprs:false_exprs
      and tail        = Ssa.create_block state.funcn in
      ignore (append head ~opcode:(Ssa.JumpIfInstr (cond, tail, false_entry)));
      ignore (append false_exit ~opcode:(Ssa.JumpInstr tail));
      tail, append tail ~ty:(Rt.tvar_as_ty ())
                        ~opcode:(Ssa.PhiInstr [head,       Ssa.const Rt.Nil;
                                               false_exit, false_value]))

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

  | Syntax.Or  (_, lhs, rhs) | Syntax.OrAssign  (_, lhs, rhs)
  | Syntax.And (_, lhs, rhs) | Syntax.AndAssign (_, lhs, rhs)
  -> (let head, lhs_value = ssa_of_expr ~state ~entry ~expr:lhs in
      let       rhs_pred  = Ssa.create_block state.funcn in
      let body, rhs_value = ssa_of_expr ~entry:rhs_pred ~state ~expr:rhs in
      let tail            = Ssa.create_block state.funcn in
      ignore (append body ~opcode:(Ssa.JumpInstr tail));
      let if_true, if_false =
        match expr with
        | Syntax.Or  _ | Syntax.OrAssign _  -> tail, rhs_pred
        | Syntax.And _ | Syntax.AndAssign _ -> rhs_pred, tail
        | _ -> assert false
      in
      ignore (append head ~opcode:(Ssa.JumpIfInstr (lhs_value, if_true, if_false)));
      let entry, value =
        tail, append tail ~ty:(Rt.tvar_as_ty ())
                          ~opcode:(Ssa.PhiInstr [head, lhs_value;
                                                 body, rhs_value]) in
      match expr with
      | Syntax.Or _ | Syntax.And _
      -> entry, value
      | Syntax.OrAssign _ | Syntax.AndAssign _
      -> ssa_of_assign ~state ~entry ~lhs ~value
      | _
      -> assert false)

  | Syntax.Not (_, expr)
  -> (let entry, value = ssa_of_expr ~state ~entry ~expr in
      entry, append entry ~ty:Rt.BooleanTy
                          ~opcode:(Ssa.PrimitiveInstr ("bool_neg", [value])))

  (* Miscellanea. *)
  | Syntax.Lambda (_, formal_args, ty, expr) (* TODO lam_ty *)
  -> ssa_of_lambda_expr ~state ~entry ~formal_args ~expr

  | Syntax.Update (_, expr)
  ->  (* Initiate functional update mode by switching the function
         of @var assignment syntax. *)
     (assert (state.update = None);
      state.update <- Some (load entry "self");
      (* Translate the body with modified @vars. *)
      let entry, _ = ssa_of_expr ~state ~entry ~expr in
      (* Extract the update result and restore @var mode. *)
      match state.update with
      | Some value
      -> (state.update <- None;
          entry, value)
      | _
      -> assert false)

  | Syntax.InvokePrimitive (_, name, operands)
  -> (let entry, operands = ssa_of_exprs ~state ~entry ~exprs:operands in
      entry, append entry ~ty:(Rt.tvar_as_ty ())
                          ~opcode:(Ssa.PrimitiveInstr (name, List.rev operands)))

  | Syntax.Quote (_, _, _)
  -> failwith ("cannot ssa_gen " ^
               (Unicode.assert_utf8s
                (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_expr expr))));

  | Syntax.Class (_, _, _, _, _) | Syntax.DefMethod (_, _, _, _, _)
  | Syntax.DefSelfMethod (_, _, _, _, _) | Syntax.DefIVar (_, _, _, _)
  -> failwith "no metaprogramming in ssa"

and ssa_of_exprs ~state ~entry ~exprs =
  List.fold_left
    (fun (entry, names) expr ->
      let entry, name = ssa_of_expr ~state ~entry ~expr in
        entry, name :: names)
    (entry, []) exprs

and ssa_of_seq ~state ~entry ~exprs =
  let entry, exprs = ssa_of_exprs ~state ~entry ~exprs in
  match exprs with
  | []
  | { Ssa.ty = Rt.NilTy } :: _
  -> entry, Ssa.const Rt.Nil
  | expr :: _
  -> entry, expr

and ssa_of_type ~state ~entry ~expr =
  match expr with
  | Syntax.TypeConstr (_, name, args)
  -> (let ty = Rt.cenv_lookup state.const_env name in
      let klass, specz = Typing.unfold_equiv ty in
      let specz = Assoc.map specz ~f:(fun _ -> Ssa.const) in
      let rec combine entry params specz args =
        match args with
        | Syntax.TypeArg (_, ty) :: args
        -> (let (name, _), params = Assoc.pluck params in
            let entry, ty = ssa_of_type ~state ~entry ~expr:ty in
            let specz = Assoc.add specz name ty in
            combine entry params specz args)
        | Syntax.TypeKwArg (_, name, ty) :: args
        -> (let entry, ty = ssa_of_type ~state ~entry ~expr:ty in
            let specz = Assoc.add specz name ty in
            combine entry params specz args)
        | []
        -> specz
      in
      let specz = combine entry klass.Rt.k_parameters specz args in
      entry, append entry ~ty:(Rt.tvar_as_ty ())
                          ~opcode:(Ssa.SpecializeInstr (Ssa.const ty, specz)))
  | Syntax.TypeSplice (_, expr)
  -> ssa_of_expr ~state ~entry ~expr

  | Syntax.TypeTuple (_, _) | Syntax.TypeRecord (_, _)
  | Syntax.TypeFunction (_, _, _) | Syntax.TypeVar (_, _)
  -> failwith ("cannot ssa_gen " ^
               (Unicode.assert_utf8s
                (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_ty expr))));

and ssa_of_pattern ~state ~entry ~pattern ~expr =
  match pattern with
  | Syntax.PatVariable (_, (kind, name))
  -> (let ty = Rt.tvar_as_ty () in
      Table.set state.frame_ty.Rt.e_ty_bindings name {
        Rt.b_ty_location = Location.empty;
        Rt.b_ty_kind     = kind;
        Rt.b_ty          = ty;
      };
      let entry, expr = ssa_of_expr ~state ~entry ~expr in
      ignore (append entry ~opcode:(Ssa.LVarStoreInstr (state.frame, name, expr)));
      entry, expr)

  | Syntax.PatTuple (_, _) | Syntax.PatRecord (_, _)
  -> failwith ("cannot ssa_gen " ^
               (Unicode.assert_utf8s
                (Sexplib.Sexp.to_string_hum (Syntax.sexp_of_pattern pattern))));

and ssa_of_actual_args ~state ~entry ~receiver ~actual_args =
  let args = Ssa_interp.append Ssa_interp.empty (Ssa_interp.Elem receiver) in
  let entry, args, kwargs =
    List.fold_left (fun (entry, args, kwargs) actual_arg ->
        match actual_arg with
        | Syntax.ActualArg (_, expr)
        -> (let entry, value = ssa_of_expr ~state ~entry ~expr in
            entry, Ssa_interp.append args (Ssa_interp.Elem value), kwargs)
        | Syntax.ActualSplice (_, expr)
        -> (let entry, value = ssa_of_expr ~state ~entry ~expr in
            entry, Ssa_interp.append args (Ssa_interp.Splice value), kwargs)
        | Syntax.ActualKwArg (_, name, expr)
        -> (let name = Ssa.const (Rt.Symbol name) in
            let entry, value = ssa_of_expr ~state ~entry ~expr in
            entry, args, Ssa_interp.append kwargs (Ssa_interp.Elem (name, value)))
        | Syntax.ActualKwPunArg (_, lvar)
        -> (let name  = Ssa.const (Rt.Symbol lvar) in
            let value = load ~state entry lvar in
            entry, args, Ssa_interp.append kwargs (Ssa_interp.Elem (name, value)))
        | Syntax.ActualKwPair (_, name_expr, value_expr)
        -> (let entry, name  = ssa_of_expr ~state ~entry ~expr:name_expr  in
            let entry, value = ssa_of_expr ~state ~entry ~expr:value_expr in
            entry, args, Ssa_interp.append kwargs (Ssa_interp.Elem (name, value)))
        | Syntax.ActualKwSplice (_, expr)
        -> (let entry, value = ssa_of_expr ~state ~entry ~expr in
            entry, args, Ssa_interp.append kwargs (Ssa_interp.Splice value)))
      (entry, args, Ssa_interp.empty) actual_args
  in
  entry, (Ssa_interp.tup_apply args entry), (Ssa_interp.rec_apply kwargs entry)

and ssa_of_formal_args ~state ~entry ~args ~arg_names =
  List.fold_left2 (fun entry arg arg_name ->
      let ty = Rt.tvar_as_ty () in
      let entry, value =
        match arg.Rt.la_default with
        | None
        -> entry, arg_name
        | Some expr
        -> (let if_none = Ssa.create_block state.funcn
            and if_any  = Ssa.create_block state.funcn
            and tail    = Ssa.create_block state.funcn in
            (* Check if we have an incoming argument. *)
            let has_arg = append entry ~ty:Rt.BooleanTy
                                       ~opcode:(Ssa.PrimitiveInstr ("opt_any", [arg_name])) in
            ignore (append entry ~opcode:(Ssa.JumpIfInstr (has_arg, if_any, if_none)));
            (* We have an argument, extract it. *)
            let any_value = append if_any ~ty
                                          ~opcode:(Ssa.PrimitiveInstr ("opt_get", [arg_name])) in
            ignore (append if_any ~opcode:(Ssa.JumpInstr (tail)));
            (* Nope, execute the default expression. *)
            let if_none, none_value = ssa_of_expr ~state ~entry:if_none ~expr in
            ignore (append if_none ~opcode:(Ssa.JumpInstr tail));
            (* Gather the value. *)
            tail, append tail ~ty ~opcode:(Ssa.PhiInstr [if_none, none_value;
                                                         if_any,  any_value]))
      in
      Table.set state.frame_ty.Rt.e_ty_bindings arg.Rt.la_name {
        Rt.b_ty_location = arg.Rt.la_location;
        Rt.b_ty_kind     = arg.Rt.la_kind;
        Rt.b_ty          = ty;
      };
      ignore (append entry ~opcode:(Ssa.LVarStoreInstr (state.frame, arg.Rt.la_name, value)));
      entry)
    entry args arg_names

and ssa_of_lambda_expr ~state ~entry ~formal_args ~expr =
  let args = Rt.lambda_args_of_formal_args formal_args in
  let arg_ids, arg_tys, ret_ty =
    List.map (fun l_arg -> l_arg.Rt.la_name) args,
    List.map (fun l_arg ->
        match l_arg with
        | Syntax.FormalSelf _
        | Syntax.FormalArg _    | Syntax.FormalRest _
        | Syntax.FormalKwArg _  | Syntax.FormalKwRest _
        -> Rt.tvar_as_ty ()
        | Syntax.FormalOptArg _ | Syntax.FormalKwOptArg _
        -> Rt.OptionTy (Rt.tvar_as_ty ()))
      formal_args,
    Rt.tvar_as_ty ()
  in
  let funcn = Ssa.create_func ~arg_ids:("env" :: arg_ids)
                (Rt.EnvironmentTy state.frame_ty :: arg_tys) ret_ty in
  let func  = Ssa.func_of_name funcn in

  (* Register the created artifact. *)
  Ssa.add_func state.capsule funcn;

  (* Extract arguments as SSA names. *)
  let env       = List.hd func.Ssa.arguments
  and arg_names = List.tl func.Ssa.arguments in

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
      append lam_entry ~ty:(Rt.EnvironmentTy frame_ty) ~opcode:(Ssa.FrameInstr env)
    in

    (* Perform SSA conversion. *)
    let lam_state = { state with
                      funcn; frame; frame_ty;
                      type_env = Table.copy state.type_env;
                      depth    = state.depth + 1 } in

    let lam_entry = ssa_of_formal_args ~entry:lam_entry ~state:lam_state ~args ~arg_names in
    let lam_entry, lam_value = ssa_of_expr ~entry:lam_entry ~state:lam_state ~expr in
    ignore (append lam_entry ~ty:Rt.NilTy ~opcode:(Ssa.ReturnInstr lam_value));

  (* Refer to the closure in the parent function. *)
  let arg_ty_elems =
    List.map2 (fun formal_arg ty ->
        match formal_arg with
        | Syntax.FormalSelf _   -> Rt.LambdaArg ty
        | Syntax.FormalArg _    -> Rt.LambdaArg ty
        | Syntax.FormalOptArg _ -> Rt.LambdaOptArg ty
        | Syntax.FormalRest _   -> Rt.LambdaRest ty
        | Syntax.FormalKwArg (_, (_, kw))       -> Rt.LambdaKwArg (kw, ty)
        | Syntax.FormalKwOptArg (_, (_, kw), _) -> Rt.LambdaKwOptArg (kw, ty)
        | Syntax.FormalKwRest _ -> Rt.LambdaKwRest ty)
      formal_args arg_tys
  in
  entry, append entry ~ty:(Rt.LambdaTy (arg_ty_elems, ret_ty))
                      ~opcode:(Ssa.ClosureInstr (funcn, state.frame))

let name_of_lambda klass selector lambda capsule =
  let id = klass.Rt.k_name ^ ":" ^ selector in

  (* Create the function with the signature corresponding to that of lambda.*)
  let arg_ids, arg_tys, ret_ty =
    let args =
      List.map2 (fun l_arg ty_elem ->
          let id = l_arg.Rt.la_name in
          match ty_elem with
          | Rt.LambdaArg ty       | Rt.LambdaRest ty
          | Rt.LambdaKwArg (_,ty) | Rt.LambdaKwRest ty
          -> id, ty
          | Rt.LambdaOptArg ty    | Rt.LambdaKwOptArg (_,ty)
          -> id, Rt.OptionTy ty)
        lambda.Rt.l_args (fst lambda.Rt.l_ty)
    in
    List.map fst args, List.map snd args, snd lambda.Rt.l_ty
  in
  let funcn = Ssa.create_func ~id ~arg_ids arg_tys ret_ty in
  let func  = Ssa.func_of_name funcn in

  (* Register the created artifacts. *)
  Ssa.add_func   capsule funcn;
  Ssa.add_lambda capsule lambda funcn;

  (* Extract arguments as SSA names. *)
  let arg_names = func.Ssa.arguments in

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
                  update    = None; } in

    let entry = ssa_of_formal_args ~state ~entry ~args:lambda.Rt.l_args ~arg_names in
    let entry, value = ssa_of_seq ~state ~entry ~exprs:lambda.Rt.l_body in

    begin match kind with
    | ConvInitializer | ConvValueInitializer
    -> (let self = load ~state entry "self" in
        ignore (append entry ~opcode:(Ssa.ReturnInstr self)))
    | _
    -> ignore (append entry ~opcode:(Ssa.ReturnInstr value))
    end;

    funcn
