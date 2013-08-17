; RUN: %foundry_opt %s -dce | %file_check %s

@foo = function() -> nil {
entry:
; CHECK: %frame =
  %frame = environment { "x" = (0 0) immutable int, "y" = (0 0) immutable int } frame empty
; CHECK: lvar_store
  lvar_store %frame, "x", int 1
; CHECK-NOT: lvar_load
  %x = int lvar_load %frame, "x"
; CHECK-NOT: primitive "int_add"
  %y = int primitive "int_add" (int 1, int 2)

  return nil
}
