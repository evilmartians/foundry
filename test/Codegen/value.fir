; RUN: %foundry_gen %s | lli | %file_check %s
; CHECK: [DEBUG: 0x00000001]
; CHECK: [DEBUG: 0x00000002]

@c.meta.Foo = class "meta.Foo" {
  metaclass @c.Class
  ancestor @c.meta.Value
}

@c.Foo = class "Foo" {
  metaclass @c.meta.Foo
  ancestor @c.Value
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
  %foo.dyn = class @c.Foo{} primitive "obj_alloc" (class @c.meta.Foo{})
  %foo.dyn.1  = class @c.Foo{} ivar_store %foo.dyn, "x", unsigned(32) 2
  call @print(%foo.dyn.1)
  return unsigned(32) 0
}

map tvar = 6
