open Unicode.Std
open Big_int
open Ssa

let name = "Literal Coercion"

let run_on_function passmgr capsule funcn =
  iter_instrs funcn ~f:(fun instr ->
    match instr.opcode with
    | PrimitiveInstr (prim, [{ opcode = Const (Rt.Integer value) }])
      when prim = "int_coerce"
    -> (let value =
          match instr.ty with
          | Rt.UnsignedTy(width)
          -> (let coerced = mod_big_int value (shift_left_big_int unit_big_int width) in
              if coerced <> value then
                failwith "too narrow"
              else
                Some (const (Rt.Unsigned (width, coerced))))
          | _
          -> None
        in
        match value with
        | Some const -> replace_instr instr const
        | None -> ())
    | _
    -> ())
