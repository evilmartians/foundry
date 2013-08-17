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

let int_shl = int_binop (fun lhs rhs -> shift_left_big_int  lhs (int_of_big_int rhs))
let int_shr = int_binop (fun lhs rhs -> shift_right_big_int lhs (int_of_big_int rhs))

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
  (* name       side_eff  impl *)
  "debug",      (true,     debug);
  (*-- machine int and big int ------------------------- *)
  "int_add",    (false,    int_binop add_big_int);
  "int_sub",    (false,    int_binop sub_big_int);
  "int_mul",    (false,    int_binop mult_big_int);
  "int_sdiv",   (false,    int_divmod);
  "int_udiv",   (false,    int_divmod);
  "int_and",    (false,    int_binop and_big_int);
  "int_or",     (false,    int_binop or_big_int);
  "int_xor",    (false,    int_binop xor_big_int);
  "int_shl",    (false,    int_shl);
  "int_lshr",   (false,    int_shr);
  "int_ashr",   (false,    int_shr);
  "int_exp",    (false,    int_binop power_big_int_positive_big_int);
  "int_cmp",    (false,    int_binop (fun lhs rhs -> big_int_of_int (compare_big_int lhs rhs)));
  "int_eq",     (false,    int_cmpop eq_big_int);
  "int_neq",    (false,    int_cmpop (fun lhs rhs -> not (eq_big_int lhs rhs)));
  "int_ule",    (false,    int_cmpop le_big_int);
  "int_sle",    (false,    int_cmpop le_big_int);
  "int_ult",    (false,    int_cmpop lt_big_int);
  "int_slt",    (false,    int_cmpop lt_big_int);
  "int_uge",    (false,    int_cmpop ge_big_int);
  "int_sge",    (false,    int_cmpop ge_big_int);
  "int_ugt",    (false,    int_cmpop gt_big_int);
  "int_sgt",    (false,    int_cmpop gt_big_int);
]

let exists = Table.exists prim

let has_side_effects name =
  let side_eff, _ = Table.get_exn prim name in
  side_eff

let invoke name args =
  let _, impl = Table.get_exn prim name in
  impl args
