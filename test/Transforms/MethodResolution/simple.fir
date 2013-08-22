; RUN: %foundry_xfrm %s -no-roots -resolve | %file_check %s

@c.Integer = class "Integer" {
  metaclass @c.meta:Integer
  methods {
    "+" = @"Integer:+"
  }
}

; CHECK: @"Integer:+" = function ([int, int] %args, {} %kwargs) -> int {
@"Integer:+" = lambda (0 0) {
  local_env empty
  type lambda ([int, int], {}) -> int
  code (self, other) { invokeprimitive int_add (self, other) };;
}

@main = function () -> nil {
entry:
; CHECK-NOT: resolve
  %add = function ([int, int], {}) -> int resolve int 1, symbol "+"
; CHECK: %x = int call @"Integer:+"
  %x   = int call %add ([int 1, int 2], {})

; CHECK-NOT: resolve
  %add.s = function ([int, int], {}) -> int resolve int 5, symbol "+"
; CHECK: %x.s = int call @"Integer:+"
  %x.s   = int call %add.s ([int 5, int 10], {})

  return nil
}
