; RUN: %foundry_xfrm %s -no-roots -resolve -infer -specialize -infer -gdce >%t
; RUN: %foundry_gen %t | lli | %file_check %s
; CHECK: [DEBUG: 0x00000003]

@c.Fixed = class "Fixed" {
  metaclass @c.meta.Fixed
  methods {
    "+" = @"Fixed:+"
  }
}

@"Fixed:+" = lambda (0 0) {
  local_env empty
  type lambda (tvar(1), tvar(2)) -> tvar(3)
  code (self, other) { invokeprimitive int_add (self, other) };;
}

@main = function () -> unsigned(32) {
entry:
  %x = tvar(5) primitive "obj_send" (unsigned(32) 1, symbol "+", [unsigned(32) 1, unsigned(32) 2], {})
  primitive "debug" (%x)
  return unsigned(32) 0
}

map tvar = 6
