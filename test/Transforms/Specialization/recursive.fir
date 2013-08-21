; RUN: %foundry_opt %s -no-roots -specialize -specialize -o %t
; RUN: %file_check %s -input-file %t
; RUN: %foundry_gen %t | lli | %file_check %s -check-prefix EXEC
; XFAIL: *

; CHECK: [[fact:@fact[\d.]*]] = function (unsigned(32) %x, unsigned(32) %one) -> unsigned(32)
@fact = function (tvar(1) %x, tvar(1) %one) -> tvar(1) {
entry:
  %is_one = boolean primitive "int_eq" (%x, %one)
  jump_if %is_one, %ret_one, %recur
ret_one:
  return %one
recur:
  %x.1 = tvar(1) primitive "int_sub" (%x, %one)
; CHECK-NOT: }
; CHECK:     call [[fact]] (%x.1, %one)
  %fact = tvar(1) call @fact (%x.1, %one)
  %fact.r = tvar(1) primitive "int_mul" (%x, %fact)
  return %fact.r
}

@main = function () -> unsigned(32) {
entry:
  %fact5 = tvar(1) call @fact (unsigned(32) 5, unsigned(32) 1)
; EXEC: [DEBUG: 0x00000078]
  primitive "debug" (%fact5) ; 120
  return unsigned(32) 0
}
