; RUN: %foundry_opt -ordered %s | %file_check %s

@1 = environment {
  bindings {
; CHECK: "b1" = (0 0) immutable true
    "b1" = (0 0) immutable true,
; CHECK: "b2" = (0 0) immutable false
    "b2" = (0 0) immutable false,
; CHECK: "b3" = (0 0) immutable nil
    "b3" = (0 0) immutable nil,
; CHECK: "b4" = (0 0) immutable int 10
    "b4" = (0 0) immutable int 10,
; CHECK: "b5" = (0 0) immutable symbol "foo"
    "b5" = (0 0) immutable symbol "foo",
; CHECK: "b6" = (0 0) immutable unsigned(32) 10
    "b6" = (0 0) immutable unsigned(32) 10,
; CHECK: "b7" = (0 0) immutable signed(32) 10
    "b7" = (0 0) immutable signed(32) 10,
; CHECK: "b8" = (0 0) immutable [true, int 10]
    "b8" = (0 0) immutable [true, int 10],
; CHECK: "b9" = (0 0) immutable {"x" = int 10, "y" = false}
    "b9" = (0 0) immutable {"x" = int 10, "y" = false},
; CHECK: "ba" = (0 0) immutable type boolean
    "ba" = (0 0) immutable type boolean,
; CHECK: "bb" = (0 0) immutable type tvar
    "bb" = (0 0) immutable type tvar,
; CHECK: "bc" = (0 0) immutable tvar(1)
    "bc" = (0 0) immutable tvar(1)
  }
}

@main = function () -> nil {
entry:
  %1 = environment {} -> {
; CHECK: "b1" = (0 0) immutable boolean
    "b1" = (0 0) immutable boolean
  } frame @1
; CHECK: %2 = unsigned(32) primitive "int_add"
  %2 = unsigned(32) primitive "int_add" (unsigned(32) 1, unsigned(32) 1)
  return nil
}
