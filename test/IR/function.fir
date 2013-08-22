; RUN: %foundry_xfrm -ordered %s | %file_check %s

; CHECK: @id = function (unsigned(32) %n) -> unsigned(32)
@id = function (unsigned(32) %n) -> unsigned(32) {
entry:
  return %n
}

@main = function () -> nil {
entry:
; CHECK: %fib = unsigned(32) call @id (unsigned(32) 5)
  %fib = unsigned(32) call @id (unsigned(32) 5)
; CHECK: %1 = unsigned(32) call @id (unsigned(32) 5)
  unsigned(32) call @id (unsigned(32) 5)
; CHECK: call @id (unsigned(32) 5)
  call @id (unsigned(32) 5)

; CHECK: %func = function (unsigned(32)) -> unsigned(32) primitive "getfun" ()
  %func = function (unsigned(32)) -> unsigned(32) primitive "getfun" ()
; CHECK: call %func (unsigned(32) 1)
  call %func(unsigned(32) 1)

  return nil
}
