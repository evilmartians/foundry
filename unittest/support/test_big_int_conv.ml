open OUnit

open Fy_big_int

let check func str =
  assert_equal ~msg:str ~printer:Fy_big_int.string_of_big_int ~cmp:Fy_big_int.eq_big_int
               (func (Unicode.assert_utf8s str))

let test_big_int_of_hex_string () =
  let check = check Big_int_conv.big_int_of_hex_string in
  check "0"         zero_big_int;
  check "1"         unit_big_int;
  check "FFFF"      (big_int_of_int 0xffff);
  check "bcd0123"   (add_big_int (shift_left_big_int (big_int_of_int 0x0bcd) 16)
                                 (big_int_of_int 0x0123));
  check "abcd0123"  (add_big_int (shift_left_big_int (big_int_of_int 0xabcd) 16)
                                 (big_int_of_int 0x0123))

let test_big_int_of_binary_string () =
  let check = check Big_int_conv.big_int_of_binary_string in
  check "0"         zero_big_int;
  check "1"         unit_big_int;
  check "1010"      (big_int_of_int 0xa);
  check "11111010"  (big_int_of_int 0xfa)

let suite = "Big_int conversion" >::: [
  "big_int_of_hex_string"    >:: test_big_int_of_hex_string;
  "big_int_of_binary_string" >:: test_big_int_of_binary_string;
]
