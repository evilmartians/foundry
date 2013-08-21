; RUN: %foundry_gen %s -o - | lli | %file_check %s

@env2 = environment {
  bindings {
    "x" = (0 0) mutable unsigned(32) 10
  }
}

@env1 = environment {
  parent @env2
  bindings {
    "y" = (0 0) mutable unsigned(32) 20
  }
}

@main = function () -> unsigned(32) {
entry:
  %frame = environment {
    "z" = (0 0) mutable unsigned(32)
  } -> {
    "y" = (0 0) mutable unsigned(32)
  } -> {
    "x" = (0 0) mutable unsigned(32)
  } frame @env1

; CHECK: [DEBUG: 0x0000000a]
  %x = unsigned(32) lvar_load %frame, "x"
  primitive "debug" (%x)
; CHECK: [DEBUG: 0x00000014]
  %y = unsigned(32) lvar_load %frame, "y"
  primitive "debug" (%y)

  lvar_store %frame, "x", unsigned(32) 1
  lvar_store %frame, "y", unsigned(32) 2
  lvar_store %frame, "z", unsigned(32) 3

; CHECK: [DEBUG: 0x00000001]
  %x.1 = unsigned(32) lvar_load %frame, "x"
  primitive "debug" (%x.1)
; CHECK: [DEBUG: 0x00000002]
  %y.1 = unsigned(32) lvar_load %frame, "y"
  primitive "debug" (%y.1)
; CHECK: [DEBUG: 0x00000003]
  %z.1 = unsigned(32) lvar_load %frame, "z"
  primitive "debug" (%z.1)

  return unsigned(32) 0
}
