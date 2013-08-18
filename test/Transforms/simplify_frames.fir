; RUN: %foundry_opt %s -simplify-frames | %file_check %s

@env1 = environment {
  bindings {
    "x" = (0 0) immutable int 10,
    "z" = (0 0) immutable int 5
  }
}

@env = environment {
  parent @env1
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
  } -> {
    "x" = (0 0) immutable int,
    "z" = (0 0) immutable int
  } frame environment @env

; CHECK-NOT: %x = int lvar_load %frame, "x"
  %x = int lvar_load %frame, "x"
; CHECK: %y = int lvar_load %frame, "y"
  %y = int lvar_load %frame, "y"
; CHECK-NOT: %z = int lvar_load %frame, "z"
  %z = int lvar_load %frame, "z"

; CHECK: lvar_store %frame, "y", int 20
  lvar_store %frame, "y", int 20

; CHECK: %r = int primitive "int_add" (int 1, %y)
  %r = int primitive "int_add" (%x, %y)
; CHECK: %r.1 = int primitive "int_add" (%r, int 5)
  %r.1 = int primitive "int_add" (%r, %z)

  return nil
}
