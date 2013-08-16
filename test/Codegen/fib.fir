; RUN: %foundry_as %s -o - | lli | %file_check %s

@fib = function (unsigned(32) %n) -> unsigned(32) {
entry:
  %is_one = boolean primitive "int_ule" (%n, unsigned(32) 2)
  jump_if %is_one, %ret_one, %recur
ret_one:
  return unsigned(32) 1
recur:
  %a = unsigned(32) primitive "int_sub" (%n, unsigned(32) 1)
  %b = unsigned(32) primitive "int_sub" (%n, unsigned(32) 2)
  %fib.a = unsigned(32) call @fib (%a)
  %fib.b = unsigned(32) call @fib (%b)
  %r = unsigned(32) primitive "int_add" (%fib.a, %fib.b)
  return %r
}

; CHECK: [DEBUG: 0x00000005]
; CHECK: [DEBUG: 0x00000037]
@main = function () -> unsigned(32) {
entry:
  %fib5 = unsigned(32) call @fib (unsigned(32) 5)
  primitive "debug" (%fib5) ; 5
  %fib10 = unsigned(32) call @fib (unsigned(32) 10)
  primitive "debug" (%fib10) ; 55
  return unsigned(32) 0
}
