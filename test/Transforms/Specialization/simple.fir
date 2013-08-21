; RUN: %foundry_opt %s -no-roots -specialize -infer -o %t
; RUN: %file_check %s -input-file %t
; RUN: %foundry_gen %t | lli | %file_check %s -check-prefix EXEC
; XFAIL: *

@add = function (tvar(1) %x, tvar(1) %y) -> tvar(1) {
entry:
  %r = tvar(1) primitive "int_add" (%x, %y)
  return %r
}

; Verify that a specialized @add is added.
; CHECK:      [[add:@add[.\d]*]] = function (unsigned(32) %x, unsigned(32) %y) -> unsigned(32) {
; CHECK-NEXT: entry:
; CHECK-NEXT:   %r = unsigned(32) primitive "int_add" (%x, %y)
; CHECK-NEXT:   return %r
; CHECK-NEXT: }

; Verify that main is updated.
; CHECK: @main = function
@main = function () -> unsigned(32) {
entry:
; CHECK: %x = unsigned(32) call [[add]] (unsigned(32) 1, unsigned(32) 2)
  %x = tvar(2) call @add (unsigned(32) 1, unsigned(32) 2)
; CHECK: %y = unsigned(32) primitive "int_add" (%x, unsigned(32) 3)
  %y = tvar(2) primitive "int_add" (%x, unsigned(32) 3)
; EXEC:  [DEBUG: 0x00000006]
  primitive "debug" (%y)
  return unsigned(32) 0
}
