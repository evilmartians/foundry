; RUN: %foundry_xfrm %s -no-roots -simplify-cfg | %file_check %s

; CHECK:      @foo = function "foo" () -> nil {
; CHECK-NEXT: entry:
; CHECK-NEXT:   return nil
; CHECK-NEXT: }
@foo = function () -> nil {
entry:
  jump %loop

loop:                                              ; preds = %incr, %entry
  jump %exit

incr:
  %x.add = int primitive "int_add" (int 1, int 1)
  jump %loop

exit:                                              ; preds = %loop
  return nil
}
