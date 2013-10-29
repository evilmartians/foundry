; RUN: %foundry_xfrm %s -no-roots -resolve | %file_check %s

@c.Integer = class "Integer" {
  metaclass @c.meta.Integer
  methods {
    "+" = @"Integer$+"
  }
}

; CHECK: @Integer$add = function "Integer$add" (int %self, int %other) -> int {
@"Integer$+" = lambda (0 0) {
  local_env empty
  type lambda (int, int) -> int
  code (self, other) { invokeprimitive int_add (self, other) };;
}

@main = function () -> nil {
entry:
; CHECK-NOT: primitive "obj_send"
; CHECK:     int call @Integer$add
  %x = int primitive "obj_send" (int 1, symbol "+", [int 1, int 2], {})

  return nil
}
