open Unicode.Std
open Ssa

type tuple  = Ssa.name
type record = Ssa.name * Ssa.name

type 'a elem =
| Elem   of 'a
| Splice of name

type 'a t = 'a elem list

let empty =
  []

let append interp elem =
  elem :: interp

let apply init comb_bare comb_packed interp entry =
  let fold (bare, packed) elem =
    match elem, bare with
    | Elem elem, _
    -> elem :: bare, packed
    | Splice splice, []
    -> [], comb_packed entry packed splice
    | Splice splice, _
    -> [], comb_packed entry (comb_bare entry packed (List.rev bare)) splice
  in
  let bare, packed = List.fold_left fold ([], init) (List.rev interp) in
  match bare with
  | [] -> packed
  | _  -> comb_bare entry packed (List.rev bare)

let ssa_append ~opcode blockn =
  let instr = create_instr (Rt.Tvar (Rt.new_tvar ())) opcode in
  append_instr instr blockn;
  instr

let tup_apply =
  let comb_bare entry packed elems =
    ssa_append entry ~opcode:(Ssa.TupleExtendInstr (packed, elems))
  in
  let comb_packed entry packed splice =
    ssa_append entry ~opcode:(Ssa.TupleConcatInstr (packed, splice))
  in
  apply (Ssa.const (Rt.Tuple [])) comb_bare comb_packed

let rec_apply =
  let comb_bare entry packed elems =
    ssa_append entry ~opcode:(Ssa.RecordExtendInstr (packed, elems))
  in
  let comb_packed entry packed splice =
    ssa_append entry ~opcode:(Ssa.RecordConcatInstr (packed, splice))
  in
  apply (Ssa.const (Rt.Record Assoc.empty)) comb_bare comb_packed
