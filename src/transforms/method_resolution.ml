open Unicode.Std
open Ssa

let run_on_function capsule funcn =
  iter_instrs funcn ~f:(fun instr ->
    match instr.opcode with
    | ResolveInstr (recvn, seln)
    -> (* Find all resolve instructions where:
           - Selector is fully known;
           - Type of receiver is at least partially known, i.e.
             not a bare type variable. Method lookup in Foundry is
             only dependent on the type itself and not its parameters. *)
       (match recvn, seln with
        | { ty = Rt.Tvar _ }, _
        -> ()
        | { ty }, { opcode = Const (Rt.Symbol selector) }
        -> (let klass   = Rt.klass_of_type ty in
            let imethod = Table.get_exn klass.Rt.k_methods selector in
            let callee =
              (* Only translate each method once. *)
              match lookup_lambda capsule imethod.Rt.im_body with
              | Some callee -> callee
              | None
              -> (let lambda  = imethod.Rt.im_body in
                  let callee  = Ssa_gen.name_of_lambda
                                  ~id:(klass.Rt.k_name ^ ":" ^ selector) lambda in
                  add_func   capsule callee;
                  add_lambda capsule imethod.Rt.im_body callee;
                  callee)
            in
            replace_instr instr callee)
        | _, _
        -> ())
    | _
    -> ())

let run_on_capsule capsule =
  List.iter (run_on_function capsule) capsule.functions
