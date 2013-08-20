; RUN: %foundry_opt %s -no-roots -inline | %file_check %s

@add = function (int %x, int %y) -> int {
entry:
  %r = int primitive "int_add" (%x, %y)
  return int 1
}

; CHECK: @b = function
@b = function () -> nil {
entry:
; CHECK-NOT: int call @add
; CHECK: %[[result:%[\w\d.]+]] = int primitive "int_add" (int 1, int 2)
  %x = int call @add (int 1, int 2)
; CHECK: primitive "debug" ([[result]])
  primitive "debug" (%x)
  return nil
}
