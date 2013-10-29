; RUN: %foundry_xfrm %s -no-roots -coerce | %file_check %s

; CHECK: @a = function "a" () -> nil {
@a = function () -> nil {
entry:
; CHECK-NOT: primitive "int_coerce"
  %x = unsigned(32) primitive "int_coerce" (int 10)
; CHECK: primitive "debug" (unsigned(32) 10)
  primitive "debug" (%x)
  return nil
}

map tvar = 0
