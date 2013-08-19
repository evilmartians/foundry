; RUN: %foundry_opt %s -no-roots -infer | %file_check %s

@a = function () -> int {
entry:
  return int 1
}

; CHECK: @b = function
@b = function () -> nil {
entry:
; CHECK: int call @a ()
  tvar(1) call @a ()
  return nil
}
