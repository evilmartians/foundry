; RUN: %foundry_xfrm %s | %file_check %s

; CHECK: @1 = function "" () -> nil
@1 = function "" () -> nil {
entry:
; CHECK: %1 = unsigned(32) primitive
  %1 = unsigned(32) primitive "int_add" (unsigned(32) 2, unsigned(32) 2)
  return nil
}
