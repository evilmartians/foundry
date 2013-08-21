open Unicode.Std

type ssa_conv_state = {
  lambda      : Rt.lambda;
  current_env : Ssa.name;
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
  match ty with
  | Rt.EnvironmentTy env -> lookup env
  | _ -> assert false

let append blockn ~ty ~opcode =
  let instr = Ssa.create_instr ty opcode in
  Ssa.append_instr instr blockn;
  instr

let ssa_of_formal_args ~entry =
  ()

let rec ssa_of_expr ~entry ~state ~expr =
  match expr with
  | Syntax.Begin (_, exprs)
  -> (let entry, names = ssa_of_exprs ~entry ~state ~exprs in
        entry, List.hd names)
  | Syntax.Int (_, value)
  -> entry, Ssa.name_of_value (Rt.Integer value)
  | Syntax.Unsigned (_, width, value)
  -> entry, Ssa.name_of_value (Rt.Unsigned (width, value))
  | Syntax.Signed (_, width, value)
  -> entry, Ssa.name_of_value (Rt.Signed (width, value))
  | Syntax.Var (_, name)
  -> entry, append entry
                ~ty:(lvar_type state.current_env.Ssa.ty name)
                ~opcode:(Ssa.LVarLoadInstr (state.current_env, name))
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
  | [] -> entry, [Ssa.name_of_value Rt.Nil]
  | _ ->
    (List.fold_left
      (fun (entry, names) expr ->
        let entry, name = ssa_of_expr ~entry ~state ~expr in
          entry, name :: names)
      (entry, [])
      exprs)

let name_of_lambda ?(id="") lambda =
  let func =
    let ty = lambda.Rt.l_ty in
      Ssa.create_func ~id
        ~arg_ids:["args"; "kwargs"]
        [ty.Rt.l_args_ty; ty.Rt.l_kwargs_ty]
        ty.Rt.l_result_ty
  in
  let entry = Ssa.create_block ~id:"entry" func in
    let current_env =
      let parent_env =
        (Rt.Environment lambda.Rt.l_local_env)
      in
      let parent_env_ty =
        match Rt.type_of_value parent_env with
        | Rt.EnvironmentTy ty -> ty
        | _ -> assert false
      in
      append entry
        ~ty:(Rt.EnvironmentTy {
          Rt.e_parent_ty   = Some parent_env_ty;
          Rt.e_bindings_ty = Table.create [];
        })
        ~opcode:(Ssa.FrameInstr (Ssa.name_of_value parent_env))
    in
    let state = { lambda; current_env; }
    in
    failwith ((u(Sexplib.Sexp.to_string_hum (Syntax.sexp_of_formal_args lambda.Rt.l_args))) ^ " " ^ (u(Sexplib.Sexp.to_string_hum (Syntax.sexp_of_exprs lambda.Rt.l_body))));
    let entry, names = ssa_of_exprs ~entry ~state ~exprs:lambda.Rt.l_body in
      ignore (append entry ~ty:Rt.NilTy ~opcode:(Ssa.ReturnInstr (List.hd names)));
      func
