; RUN: %foundry_opt %s -no-roots -inline | %file_check %s

@a = function () -> int {
entry:
  return int 1
}

; CHECK: @b = function
@b = function () -> nil {
entry:
; CHECK-NOT: int call @a ()
  %x = int call @a ()
; CHECK: primitive "debug" (int 1)
  primitive "debug" (%x)
  return nil
}
