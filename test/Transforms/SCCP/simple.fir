; RUN: %foundry_xfrm %s -no-roots -sccp | %file_check %s

@foo = function() -> nil {
; CHECK:      entry:
; CHECK-NEXT:   return int 7
entry:
  %x = int primitive "int_add" (int 1, int 2)
  %y = int primitive "int_sub" (int 10, %x)
  return %y
}
