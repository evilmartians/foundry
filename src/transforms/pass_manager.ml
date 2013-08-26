open Unicode.Std

type t =
| Sequental of seq_passes
| Worklist  of dep_passes
and seq_passes = {
  mutable passes          : (string * (Ssa.capsule -> unit)) list;
}
and dep_passes = {
  mutable capsule_passes  : (string * (Ssa.capsule -> unit)) list;
  mutable function_passes : (string * (Ssa.capsule -> Ssa.name -> unit)) list;
  mutable worklist        : Ssa.name Worklist.t;
}

module type FunctionPass =
sig
  val name            : string
  val run_on_function : t -> Ssa.capsule -> Ssa.name -> unit
end

module type CapsulePass =
sig
  val name            : string
  val run_on_capsule  : t -> Ssa.capsule -> unit
end

let verbose = ref false
let level   = ref 0

let print_verbose str =
  if !verbose then
    prerr_endline ((String.make (!level * 2) ' ') ^ str)

let print_invalidate id reason =
  match reason with
  | None   -> print_verbose ("Invalidating @" ^ id ^ ".")
  | Some r -> print_verbose ("Invalidating @" ^ id ^ ": " ^ r ^ ".")

let print_exn exn reason =
  let exn_desc =
    match exn with
    | Typing.Conflict (a, b)
    -> "Typing.Conflict (" ^ (Rt.inspect_type a) ^
          ", " ^ (Rt.inspect_type b) ^ ")"
    | _
    -> Unicode.assert_utf8s (Printexc.to_string exn)
  in
  print_verbose ("Exception: " ^ exn_desc);
  print_verbose ("Reason: " ^ reason);
  if !verbose then raise Exit else raise exn

let create ~sequental =
  if sequental
  then Sequental {
    passes = []
  }
  else Worklist {
    capsule_passes  = [];
    function_passes = [];
    worklist        = Worklist.create ();
  }

let run' passmgr capsule =
  let run_capsule (name, pass) =
    print_verbose ("Entering module pass '" ^ name ^ "'.");
    pass capsule;
    print_verbose ("Leaving module pass '" ^ name ^ "'.")
  in
  let run_function funcn (name, pass) =
    print_verbose ("Entering function pass '" ^ name ^ "' for @" ^ funcn.Ssa.id ^ ".");
    pass capsule funcn;
    print_verbose ("Leaving function pass '" ^ name ^ "'.")
  in
  match passmgr with
  | Sequental { passes }
  -> List.iter run_capsule passes
  | Worklist passmgr'
  -> ((* Set a sigint signal to allow premature termination. *)
      let terminate  = ref false in
      let old_sigint = Sys.signal Sys.sigint
            (Sys.Signal_handle (fun _ -> terminate := true)) in

      (* Initially, put all functions in the worklist. *)
      Ssa.iter_funcs capsule ~f:(Worklist.put passmgr'.worklist);

      (* Repeat until the whole program converges. *)
      while Worklist.some passmgr'.worklist && not !terminate do
        (* Worklist will be mutated by the transformations, so
           work off a copy. *)
        let worklist = Worklist.copy passmgr'.worklist in
        passmgr'.worklist <- Worklist.create ();

        (* Converge all function passes (inference, etc). *)
        while Worklist.some worklist && not !terminate do
          let funcn = Worklist.take worklist in
          List.iter (run_function funcn) passmgr'.function_passes
        done;

        (* Perform all capsule passes (specialization, etc). *)
        List.iter run_capsule passmgr'.capsule_passes;
      done;

      if !terminate then
        prerr_endline "Convergence terminated by ^C.";

      (* Restore the old signal. This also makes the pass manager
         reentrant. *)
      Sys.set_signal Sys.sigint old_sigint)

let run passmgr capsule =
  try
    run' passmgr capsule
  with
  | Exit ->
    ()
  | exn ->
    prerr_endline "Convergence terminated by uncaught exception";
    if not !verbose then begin
      prerr_endline (Unicode.assert_utf8s (Printexc.to_string exn));
      Printexc.print_backtrace stderr
    end

let mark passmgr ?reason funcn =
  print_invalidate funcn.Ssa.id reason;
  match passmgr with
  | Worklist { worklist }
  -> Worklist.put worklist funcn
  | _
  -> ()

let add_capsule_pass passmgr pass =
  let module Pass = (val pass : CapsulePass) in
  match passmgr with
  | Sequental passmgr'
  -> passmgr'.passes <- passmgr'.passes @
        [Pass.name,
         Pass.run_on_capsule passmgr]
  | Worklist passmgr'
  -> passmgr'.capsule_passes <- passmgr'.capsule_passes @
        [Pass.name,
         Pass.run_on_capsule passmgr]

let add_function_pass passmgr pass =
  let module Pass = (val pass : FunctionPass) in
  match passmgr with
  | Sequental passmgr'
  -> passmgr'.passes <- passmgr'.passes @
        [Pass.name,
         fun capsule -> Ssa.iter_funcs capsule ~f:(Pass.run_on_function passmgr capsule)]
  | Worklist passmgr'
  -> passmgr'.function_passes <- passmgr'.function_passes @
        [Pass.name,
         Pass.run_on_function passmgr]

let add_pass_manager passmgr inner_passmgr =
  let passmgr_pass =
    "Nested Pass Manager",
    (fun capsule ->
      level := !level + 1;
      run' inner_passmgr capsule;
      level := !level - 1)
  in
  match passmgr with
  | Sequental passmgr'
  -> passmgr'.passes <- passmgr'.passes @ [passmgr_pass]
  | Worklist passmgr'
  -> passmgr'.capsule_passes <- passmgr'.capsule_passes @ [passmgr_pass]
