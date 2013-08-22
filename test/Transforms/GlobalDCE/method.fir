; RUN: %foundry_opt %s -no-roots -resolve -specialize -gdce | %file_check %s
; CHECK-NOT: map lambda

@c.Unsigned = class "Unsigned" {
  metaclass @c.meta:Unsigned
  methods {
    "+" = @"Unsigned:+"
  }
}

@"Unsigned:+" = lambda (0 0) {
  local_env empty
  type lambda (tvar(1), tvar(2)) -> tvar(3)
  code (self, other) { invokeprimitive int_add (self, other) };;
}

@main = function () -> unsigned(32) {
entry:
  %add = tvar(4) resolve unsigned(32) 1, symbol "+"
  %x   = tvar(5) call %add ([unsigned(32) 1, unsigned(32) 2], {})
  primitive "debug" (%x)
  return unsigned(32) 0
}

map tvar = 6
