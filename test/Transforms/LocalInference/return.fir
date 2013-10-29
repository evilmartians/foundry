; RUN: %foundry_xfrm %s -no-roots -ordered -infer | %file_check %s

; CHECK: @a = function "a" ([int] %x) -> int
@a = function ([tvar(1)] %x) -> tvar(1) {
entry:
  return int 1
}

; CHECK: @b = function "b" (int %x, int %y) -> int
@b = function (tvar(2) %x, tvar(2) %y) -> int {
entry:
; CHECK: %z = int primitive
  %z = tvar(2) primitive "int_add" (%x, %y)
  return %z
}
