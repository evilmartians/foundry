module Nat     = Fy_nat
module Big_int = Fy_big_int

let prepare str =
  let src = (str : Unicode.utf8s :> string)   in
  let dst = String.create (String.length src) in
  let rec blit src_idx dst_idx =
    if src_idx < String.length src then
      match src.[src_idx] with
      | '_' -> blit (src_idx + 1) dst_idx
      | c   -> dst.[dst_idx] <- c; blit (src_idx + 1) (dst_idx + 1)
    else
      String.sub dst 0 dst_idx
  in
  blit 0 0

let big_int_of_bin_string str =
  let str = prepare str in
  let nat = Nat.sys_nat_of_string 2 str 0 (String.length str) in
  Big_int.big_int_of_nat nat

let big_int_of_dec_string str =
  let str = prepare str in
  let nat = Nat.sys_nat_of_string 10 str 0 (String.length str) in
  Big_int.big_int_of_nat nat

let big_int_of_hex_string str =
  let str = prepare str in
  (* sys_nat_of_string only understands uppercase. *)
  let str = String.uppercase str in
  let nat = Nat.sys_nat_of_string 16 str 0 (String.length str) in
  Big_int.big_int_of_nat nat
