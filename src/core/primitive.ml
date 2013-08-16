open Unicode.Std
open Big_int
open Rt

let int_binop op =
  let f args =
    match args with
    | [Unsigned(wl,lhs); Unsigned(wr,rhs)] when wl = wr
    -> Unsigned(wl, mod_big_int (op lhs rhs) (big_int_of_int wl))
    | [Integer(lhs); Integer(rhs)]
    -> Integer(op lhs rhs)
    | _ -> assert false
  in f

let int_cmpop op =
  let f args =
    match args with
    | [Unsigned(wl,lhs); Unsigned(wr,rhs)]
    | [  Signed(wl,lhs);   Signed(wr,rhs)]
      when wl = wr
    -> if op lhs rhs then Rt.Truth else Rt.Lies
    | [Integer(lhs); Integer(rhs)]
    -> if op lhs rhs then Rt.Truth else Rt.Lies
    | _ -> assert false
  in f

let int_divmod args =
  match args with
  | [Unsigned(wl,lhs); Unsigned(wr,rhs)] when wl = wr
  -> (let quo, rem = quomod_big_int lhs rhs in
        Tuple [Unsigned(wl, quo); Unsigned(wl, rem)])
  | [Integer(lhs); Integer(rhs)]
  -> (let quo, rem = quomod_big_int lhs rhs in
        Tuple [Integer(quo); Integer(rem)])
  | _ -> assert false

let debug args =
  match args with
  | [arg] ->
    print_endline (Unicode.assert_utf8s
      (Sexplib.Sexp.to_string_hum (sexp_of_value arg)));
    Rt.Nil
  | _ -> assert false

let prim = Table.create [
  "debug",      debug;
  (*-- machine int and big int ------------------------- *)
  "int_add",    int_binop add_big_int;
  "int_sub",    int_binop sub_big_int;
  "int_mul",    int_binop mult_big_int;
  "int_sdiv",   int_divmod;
  "int_udiv",   int_divmod;
  "int_and",    int_binop and_big_int;
  "int_or",     int_binop or_big_int;
  "int_xor",    int_binop xor_big_int;
  "int_shl",    int_binop (fun lhs rhs -> shift_left_big_int  lhs (int_of_big_int rhs));
  "int_lshr",   int_binop (fun lhs rhs -> shift_right_big_int lhs (int_of_big_int rhs));
  "int_ashr",   int_binop (fun lhs rhs -> shift_right_big_int lhs (int_of_big_int rhs));
  "int_exp",    int_binop power_big_int_positive_big_int;
  "int_cmp",    int_binop (fun lhs rhs -> big_int_of_int (compare_big_int lhs rhs));
  "int_eq",     int_cmpop eq_big_int;
  "int_neq",    int_cmpop (fun lhs rhs -> not (eq_big_int lhs rhs));
  "int_ule",    int_cmpop le_big_int;
  "int_sle",    int_cmpop le_big_int;
  "int_ult",    int_cmpop lt_big_int;
  "int_slt",    int_cmpop lt_big_int;
  "int_uge",    int_cmpop ge_big_int;
  "int_sge",    int_cmpop ge_big_int;
  "int_ugt",    int_cmpop gt_big_int;
  "int_sgt",    int_cmpop gt_big_int;
]

let exists = Table.exists prim

let invoke name args =
  (Table.get_exn prim name) args
