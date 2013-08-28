open Unicode.Std
open Ssa

let name = "Method Resolution"

let run_on_function passmgr capsule caller =
  iter_instrs caller ~f:(fun instr ->
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

        (* Special-case records for a while. *)
        | { ty = Rt.RecordTy xs }, { opcode = Ssa.Const (Rt.Symbol selector) }
        -> (iter_uses instr ~f:(fun call_instr ->
              match call_instr.opcode with
              | CallInstr (_, _)
              -> (let getter = create_instr (Assoc.find xs selector)
                                            (PrimitiveInstr ("rec_lookup", [recvn; seln])) in
                  replace_instr call_instr getter)
              | _
              -> assert false);
            erase_instr instr)

        | { ty }, { opcode = Const (Rt.Symbol selector) }
        -> (let rec lookup klass selector =
              try
                klass, Assoc.find klass.Rt.k_methods selector
              with Not_found ->
                match klass.Rt.k_ancestor with
                | Some ancestor -> lookup ancestor selector
                | None -> raise Not_found
            in
            let klass = Rt.klass_of_type ty in
            let klass, imethod =
              try lookup klass selector
              with Not_found ->
                try lookup klass "method_missing"
                with Not_found -> failwith ("no method " ^ (klass.Rt.k_name) ^ "#" ^ selector)
            in
            let callee =
              (* Only translate each method once. *)
              match lookup_lambda capsule imethod.Rt.im_body with
              | Some callee -> callee
              | None
              -> (let lambda  = imethod.Rt.im_body in
                  let callee  = Ssa_gen.name_of_lambda klass selector lambda capsule in
                  Pass_manager.mark passmgr callee;
                  callee)
            in
            replace_instr instr callee)
        | _, _
        -> ())
    | _
    -> ())
