; RUN: %foundry_opt %s -no-roots -resolve | %file_check %s

@c.Integer = class "Integer" {
  metaclass @c.meta:Integer
  methods {
    "+" = @"Integer:+"
  }
}

@1 = environment {
  bindings {}
}

@"Integer:+" = lambda (0 0) {
  local_env @1
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
