; RUN: %foundry_opt %s -simplify-frames | %file_check %s

@env = environment {
  bindings {
    "x" = (0 0) immutable int 1,
    "y" = (0 0) mutable int 2
  }
}

@foo = function() -> nil {
entry:
  %frame = environment {} -> {
    "x" = (0 0) immutable int,
    "y" = (0 0) mutable int
  } frame environment @env
; CHECK-NOT: %x = int lvar_load %frame, "x"
  %x = int lvar_load %frame, "x"
; CHECK: %y = int lvar_load %frame, "y"
  %y = int lvar_load %frame, "y"
; CHECK: %r = int primitive "int_add" (int 1, %y)
  %r = int primitive "int_add" (%x, %y)

  return nil
}
