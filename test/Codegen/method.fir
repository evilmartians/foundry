; RUN: %foundry_opt %s -no-roots -resolve -specialize -gdce -infer >%t
; RUN: %foundry_gen %t | lli | %file_check %s
; CHECK: [DEBUG: 0x00000003]

@c.Unsigned = class "Unsigned" {
  metaclass @c.meta:Unsigned
  methods {
    "+" = @"Unsigned:+"
  }
}

@"Unsigned:+" = lambda (0 0) {
  local_env empty
  type lambda (tvar(10), tvar(11)) -> tvar(12)
  code (self, other) { invokeprimitive int_add (self, other) };;
}

@main = function () -> unsigned(32) {
entry:
  %add = tvar(13) resolve unsigned(32) 1, symbol "+"
  %x   = tvar(14) call %add ([unsigned(32) 1, unsigned(32) 2], {})
  primitive "debug" (%x)
  return unsigned(32) 0
}
