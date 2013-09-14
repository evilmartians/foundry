open Unicode.Std
open Big_int
open ExtList
open Rt

exception Undefined_primitive of string

(* Debug primitive implementations. *)

let debug args =
  match args with
  | [arg] -> assert false
  | _ -> assert false

(* Integer primitive implementations. *)

let int_binop op =
  (fun args ->
    match args with
    | [Unsigned(wl,lhs); Unsigned(wr,rhs)] when wl = wr
    -> (let prec = shift_left_big_int unit_big_int wl in
        Unsigned(wl, mod_big_int (op lhs rhs) prec))
    | [Integer(lhs); Integer(rhs)]
    -> Integer(op lhs rhs)
    | _ -> assert false)

let int_cmpop op =
  (fun args ->
    match args with
    | [Unsigned(wl,lhs); Unsigned(wr,rhs)]
    | [  Signed(wl,lhs);   Signed(wr,rhs)]
      when wl = wr
    -> if op lhs rhs then Rt.Truth else Rt.Lies
    | [Integer(lhs); Integer(rhs)]
    -> if op lhs rhs then Rt.Truth else Rt.Lies
    | _ -> assert false)

let int_shl = int_binop (fun lhs rhs -> shift_left_big_int  lhs (int_of_big_int rhs))
let int_shr = int_binop (fun lhs rhs -> shift_right_big_int lhs (int_of_big_int rhs))

let int_to_str args =
  match args with
  | [Integer(value)] | [Signed(_, value)] | [Unsigned(_, value)]
  -> String (string_of_big_int value)
  | _
  -> assert false

(* Tuple primitive implementations. *)

let tup_length args =
  match args with
  | [Tuple(xs)] -> Integer (big_int_of_int (List.length xs))
  | _ -> assert false

let tup_lookup args =
  match args with
  | [Tuple(xs); Integer(n)] -> List.nth xs (int_of_big_int n)
  | _ -> assert false

let tup_slice args =
  match args with
  | [Tuple(xs); Integer(lft); Integer(rgt)]
  -> (let lft, rgt = int_of_big_int lft, int_of_big_int rgt in
      let _, lft_slice = List.split_nth lft xs in
      let rgt_slice, _ = List.split_nth (rgt - lft) lft_slice in
      Tuple rgt_slice)
  | _
  -> assert false

(* Record primitive implementations. *)

let rec_lookup args =
  match args with
  | [Record(xs); Symbol(n)] -> Assoc.find xs n
  | _ -> assert false

(* Symbol primitive implementations. *)

let sym_to_str args =
  match args with
  | [Symbol str] -> String str
  | _ -> assert false

(* Object primitive implementations. *)

let obj_alloc args =
  match args with
  | [Class (cls)]
  -> (Instance({
        i_hash  = Hash_seed.make ();
        i_class = cls;
        i_slots = Table.create []
      }))
  | _
  -> assert false

let obj_equal args =
  match args with
  | [a; b] -> if equal a b then Rt.Truth else Rt.Lies
  | _ -> assert false

(* Class primitive implementations. *)

let cls_defm args =
  match args with
  | [Rt.Class (klass, _); Rt.Symbol name; Rt.Lambda body]
  -> (let meth = {
        im_hash    = Hash_seed.make ();
        im_body    = body;
        im_dynamic = false; } in
      klass.k_methods <- Assoc.append klass.k_methods name meth;
      Rt.Nil)
  | _
  -> assert false

let prim = Table.create [
  (* name       side-eff?  impl *)
  (* -- debug ------------------------------------------ *)
  "debug",      (true,     debug);
  "external",   (true,     fun _ -> assert false);
  "externalva", (true,     fun _ -> assert false);
  (* -- booleans --------------------------------------- *)
  "bool_neg",   (false,    fun _ -> assert false);
  (* -- machine int and big int ------------------------ *)
  "int_add",    (false,    int_binop add_big_int);
  "int_sub",    (false,    int_binop sub_big_int);
  "int_mul",    (false,    int_binop mult_big_int);
  "int_div",    (false,    int_binop div_big_int);
  "int_mod",    (false,    int_binop mod_big_int);
  "int_and",    (false,    int_binop and_big_int);
  "int_or",     (false,    int_binop or_big_int);
  "int_xor",    (false,    int_binop xor_big_int);
  "int_shl",    (false,    int_shl);
  "int_shr",    (false,    int_shr);
  "int_exp",    (false,    int_binop power_big_int_positive_big_int);
  "int_cmp",    (false,    int_binop (fun lhs rhs -> big_int_of_int (compare_big_int lhs rhs)));
  "int_eq",     (false,    int_cmpop eq_big_int);
  "int_ne",     (false,    int_cmpop (fun lhs rhs -> not (eq_big_int lhs rhs)));
  "int_le",     (false,    int_cmpop le_big_int);
  "int_lt",     (false,    int_cmpop lt_big_int);
  "int_ge",     (false,    int_cmpop ge_big_int);
  "int_gt",     (false,    int_cmpop gt_big_int);
  "int_coerce", (false,    fun _ -> assert false);
  "int_to_str", (false,    int_to_str);
  (* -- tuples ----------------------------------------- *)
  "tup_length", (false,    tup_length);
  "tup_lookup", (false,    tup_lookup);
  "tup_slice",  (false,    tup_slice);
  "tup_enum",   (true,     fun _ -> assert false);
  (* -- records ---------------------------------------- *)
  "rec_lookup", (false,    rec_lookup);
  "rec_enum",   (true,     fun _ -> assert false);
  (* -- symbols ---------------------------------------- *)
  "sym_to_str", (false,    sym_to_str);
  (* -- closures --------------------------------------- *)
  "lam_call",   (true,     fun _ -> assert false);
  (* -- objects ---------------------------------------- *)
  "obj_alloc",  (false,    obj_alloc);
  "obj_equal",  (false,    obj_equal);
  "obj_send",   (true,     fun _ -> assert false);
  (* -- classes ---------------------------------------- *)
  "cls_alloc",  (false,    fun _ -> assert false);
  "cls_defm",   (true,     cls_defm);
  "cls_defv",   (true,     fun _ -> assert false);
  (* -- hardware access -------------------------------- *)
  "mem_load",   (false,    fun _ -> assert false);
  "mem_store",  (true,     fun _ -> assert false);
  "mem_loadv",  (true,     fun _ -> assert false);
  "mem_storev", (true,     fun _ -> assert false);
]

let find name =
  try
    Table.get_exn prim name
  with Not_found ->
    raise (Undefined_primitive name)

let exists = Table.exists prim

let has_side_effects name =
  fst (find name)

let invoke name args =
  (snd (find name)) args
