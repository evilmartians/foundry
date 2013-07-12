open Unicode.Std
open Big_int
open Rt

let int_binop op =
  let f args =
    match args with
    | Integer(lhs), Integer(rhs) -> Integer(op lhs rhs)
    | _ -> assert false
  in f

let prim0 = Table.create []

let prim1 = Table.create []

let prim2 = Table.create [
  "int_add", int_binop add_big_int;
  "int_sub", int_binop sub_big_int;
  "int_mul", int_binop mult_big_int;
  "int_div", int_binop div_big_int;
  "int_mod", int_binop mod_big_int;
  "int_and", int_binop and_big_int;
  "int_or",  int_binop or_big_int;
  "int_xor", int_binop xor_big_int;
  "int_shl", int_binop (fun lhs rhs -> shift_left_big_int  lhs (int_of_big_int rhs));
  "int_shr", int_binop (fun lhs rhs -> shift_right_big_int lhs (int_of_big_int rhs));
  "int_cmp", int_binop (fun lhs rhs -> big_int_of_int (compare_big_int lhs rhs));
  "int_exp", int_binop power_big_int_positive_big_int;
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
