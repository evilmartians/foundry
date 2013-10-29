; RUN: %foundry_xfrm %s -no-roots -ordered -infer -specialize | %file_check %s

@c.meta.Printer = class "meta.Printer" {
  metaclass @c.Class
  objectclass @c.Printer
  parameters {
    "v" = tvar(3)
  }
}

@c.Printer = class "Printer" {
  metaclass @c.meta.Printer
  parameters {
    "v" = tvar(3)
  }
}

@print = function (class @c.meta.Printer{"v" = tvar(3)} %args) -> nil {
entry:
; CHECK: primitive "debug" (unsigned(32) 1)
  primitive "debug" (tvar(3))
  return nil
}

@main = function () -> unsigned(32) {
entry:
  call @print (class @c.Printer{"v" = unsigned(32) 1})
  return unsigned(32) 0
}

