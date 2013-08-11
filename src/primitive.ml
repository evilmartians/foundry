open Unicode.Std
open Big_int
open Rt

let int_binop op =
  let f args =
    match args with
    | [Integer(lhs); Integer(rhs)] -> Integer(op lhs rhs)
    | _ -> assert false
  in f

let debug args =
  match args with
  | [arg] ->
    print_endline (Unicode.assert_utf8s
      (Sexplib.Sexp.to_string_hum (sexp_of_value arg)));
    Rt.Nil
  | _ -> assert false

let prim = Table.create [
  (* 1-ary *)
  "debug",   debug;
  (* 2-ary *)
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

let exists = Table.exists prim

let invoke name args =
  (Table.get_exn prim name) args
