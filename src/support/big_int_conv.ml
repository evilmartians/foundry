module Nat     = Fy_nat
module Big_int = Fy_big_int

let big_int_of_hex_string str =
  let str = (str : Unicode.utf8s :> string) in
  (* sys_nat_of_string only understands uppercase. *)
  let str = String.uppercase str in
  let nat = Nat.sys_nat_of_string 16 str 0 (String.length str) in
  Big_int.big_int_of_nat nat

let big_int_of_binary_string str =
  let str = (str : Unicode.utf8s :> string) in
  let nat = Nat.sys_nat_of_string 2 str 0 (String.length str) in
  Big_int.big_int_of_nat nat
