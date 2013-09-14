; RUN: %foundry_gen %s -o - | lli | %file_check %s

; CHECK:      [DEBUG: 0x00000001]
; CHECK-NEXT: [DEBUG: 0x00000002]
; CHECK-NEXT: [DEBUG: 0x00000003]
; CHECK-NEXT: [DEBUG: 0x00000004]
; CHECK-NEXT: [DEBUG: 0x00000005]
; CHECK-NOT:  [DEBUG

@main = function () -> unsigned(32) {
entry:
  jump %loop
loop:
  %i = unsigned(32) phi [ %entry => unsigned(32) 1, %loop => %i.next ]
  primitive "debug" (%i)
  %i.next = unsigned(32) primitive "int_add" (%i, unsigned(32) 1)
  %exit_p = boolean primitive "int_ge" (%i, unsigned(32) 5)
  jump_if %exit_p, %exit, %loop
exit:
  return unsigned(32) 0
}
