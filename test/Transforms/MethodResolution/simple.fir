; RUN: %foundry_opt %s -no-roots -resolve | %file_check %s

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
  %add = function ([int, int], {}) -> int resolve int 1, symbol "+"
  %x   = int call %add ([int 1, int 2], {})
  primitive "debug" (%x)
  return nil
}
