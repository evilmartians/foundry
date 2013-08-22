open Unicode.Std
open Big_int

type ssa_conv_state = {
          lambda    : Rt.lambda;
          frame     : Ssa.name;
          frame_ty  : Rt.local_env_ty;
          args      : Ssa.name;
          kwargs    : Ssa.name;
  mutable arg_idx   : int;
}

let tvar () =
  Rt.Tvar (Rt.new_tvar ())

let lvar_type ty name =
  let rec lookup env =
    match Table.get env.Rt.e_bindings_ty name with
    | Some binding -> binding.Rt.b_value_ty
    | None
    -> (match env.Rt.e_parent_ty with
        | Some parent_ty -> lookup parent_ty
        | None -> assert false)
  in
  lookup ty

let append blockn ~ty ~opcode =
  let instr = Ssa.create_instr ty opcode in
  Ssa.append_instr instr blockn;
  instr

let rec ssa_of_expr ~entry ~state ~expr =
  match expr with
  (* Constants. *)
  | Syntax.Int (_, value)
  -> entry, Ssa.name_of_value (Rt.Integer value)
  | Syntax.Unsigned (_, width, value)
  -> entry, Ssa.name_of_value (Rt.Unsigned (width, value))
  | Syntax.Signed (_, width, value)
  -> entry, Ssa.name_of_value (Rt.Signed (width, value))
  (* Code block. *)
  | Syntax.Begin (_, exprs)
  -> (let entry, names = ssa_of_exprs ~entry ~state ~exprs in
        entry, List.hd names)
  (* Variable access and assignment. *)
  | Syntax.Self (_)
  -> entry, append entry
                ~ty:(lvar_type state.frame_ty "self")
                ~opcode:(Ssa.LVarLoadInstr (state.frame, "self"))
  | Syntax.Var (_, name)
  -> entry, append entry
                ~ty:(lvar_type state.frame_ty name)
                ~opcode:(Ssa.LVarLoadInstr (state.frame, name))
  (* Miscellanea. *)
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
  match exprs with
  | []
  -> entry, [Ssa.name_of_value Rt.Nil]
  | _
  -> (List.fold_left
        (fun (entry, names) expr ->
          let entry, name = ssa_of_expr ~entry ~state ~expr in
            entry, name :: names)
        (entry, [])
        exprs)

let ssa_of_formal_arg ~entry ~state ~arg =
  (* All bindings initially have a fresh type variable as
     their type. *)
  let ty = tvar () in
  let assign kind name value =
    Table.set state.frame_ty.Rt.e_bindings_ty name {
      Rt.b_location_ty = Location.empty;
      Rt.b_kind_ty     = kind;
      Rt.b_value_ty    = ty;
    };
    append entry ~ty:Rt.NilTy
      ~opcode:(Ssa.LVarStoreInstr (state.frame, name, value));
    entry
  in
  match arg with
  | Syntax.FormalSelf (_)
  -> (let arg =
        append entry ~ty
           ~opcode:(Ssa.PrimitiveInstr ("tup_index",
                    [state.args; (Ssa.name_of_value (Rt.Integer (big_int_of_int 0)))]))
      in
      state.arg_idx <- 1;
      assign Syntax.LVarImmutable "self" arg)
  | Syntax.FormalArg (_, (kind, name))
  -> (let arg =
        append entry ~ty
          ~opcode:(Ssa.PrimitiveInstr ("tup_index",
                   [state.args; (Ssa.name_of_value (Rt.Integer (big_int_of_int state.arg_idx)))]))
      in
      state.arg_idx <- state.arg_idx + 1;
      assign kind name arg)

let ssa_of_formal_args ~entry ~state ~args =
  List.fold_left (fun entry arg ->
      ssa_of_formal_arg ~entry ~state ~arg)
    entry args)

let name_of_lambda ?(id="") lambda =
  (* Create the function with the signature corresponding to that
     of lambda. Usually it would be (\x, \y) -> \z. *)
  let funcn =
    let ty = lambda.Rt.l_ty in
    Ssa.create_func ~id
      ~arg_ids:["args"; "kwargs"]
      [ty.Rt.l_args_ty; ty.Rt.l_kwargs_ty]
      ty.Rt.l_result_ty
  in
  let func = Ssa.func_of_name funcn in
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
      Rt.e_parent_ty   = Some (Rt.type_of_environment lambda.Rt.l_local_env);
      Rt.e_bindings_ty = Table.create [];
    }
    in
    let frame =
      append entry ~ty:(Rt.EnvironmentTy frame_ty)
          ~opcode:(Ssa.FrameInstr (Ssa.name_of_value
                    (Rt.Environment lambda.Rt.l_local_env)))
    in

    (* Perform SSA conversion. *)
    let state = { lambda; frame; frame_ty; args; arg_idx = 0; kwargs } in
    let entry =
      ssa_of_formal_args ~entry ~state ~args:lambda.Rt.l_args in
    let entry, names =
      ssa_of_exprs ~entry ~state ~exprs:lambda.Rt.l_body in
    ignore (append entry ~ty:Rt.NilTy ~opcode:(Ssa.ReturnInstr (List.hd names)));

    funcn
