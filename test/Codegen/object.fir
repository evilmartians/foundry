; RUN: %foundry_gen %s | lli | %file_check %s
; CHECK: [DEBUG: 0x00000001]
; CHECK: [DEBUG: 0x00000002]

@c.meta:Foo = class "meta:Foo" {
  metaclass @c.Class
}

@c.Foo = class "Foo" {
  metaclass @c.meta:Foo
  ivars {
    "x" = (0 0) immutable unsigned(32)
  }
}

@foo.const = instance @c.Foo{} {
  "x" = unsigned(32) 1
}

@print = function (class @c.Foo{} %foo) -> nil {
entry:
  %val = unsigned(32) ivar_load %foo, "x"
  primitive "debug" (%val)
  return nil
}

@main = function () -> unsigned(32) {
entry:
  call @print(instance @foo.const)
  %foo.dyn = class @c.Foo{} primitive "obj_alloc" (class @c.meta:Foo{})
  ivar_store %foo.dyn, "x", unsigned(32) 2
  call @print(%foo.dyn)
  return unsigned(32) 0
}

map tvar = 6
