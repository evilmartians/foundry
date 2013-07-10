open Unicode.Std
open Rt

let prim_int_add args =
  let Int(lhs), Int(rhs) = args in
    Int(lhs + rhs)

let prim0 = Table.create []

let prim1 = Table.create []

let prim2 = Table.create [
  "int_add", prim_int_add;
]

let exists name arity =
  if arity > 2 then
    false
  else
    let table =
      match arity with
      | 0 -> prim0 | 1 -> prim1 | 2 -> prim2
      | _ -> assert false
    in Table.exists table name

let invoke (name : string) (args : value list) : value =
  match args with
  | [a1; a2] -> (Table.get_exn prim2 name) (a1, a2)
  (* | [a1]     -> (Table.get_exn prim1 name) (a1) *)
  (* | []       -> (Table.get_exn prim0 name) () *)
  | _ -> assert false
