; RUN: %foundry_as %s -o - | lli | %file_check %s

; CHECK:      [DEBUG: 0x00000001]
; CHECK-NEXT: [DEBUG: 0x00000002]
; CHECK-NEXT: [DEBUG: 0x00000003]
; CHECK-NEXT: [DEBUG: 0x00000004]
; CHECK-NEXT: [DEBUG: 0x00000005]
; CHECK-NOT:  [DEBUG

@main = function () -> type unsigned(32) {
entry:
  jump %loop
loop:
  %i = type unsigned(32) phi [ %entry => unsigned(32) 1 ], [ %loop => %i.next ]
  type nil primitive "debug" (%i)
  %i.next = type unsigned(32) primitive "int_add" (%i, unsigned(32) 1)
  %exit_p = type boolean primitive "int_ugt" (%i, unsigned(32) 5)
  jump_if %exit_p, %exit, %loop
exit:
  return unsigned(32) 0
}
