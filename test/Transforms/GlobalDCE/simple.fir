; RUN: %foundry_xfrm %s -no-roots -gdce | %file_check %s

; CHECK-NOT: @"Unsigned:+.2"
; CHECK-NOT: map function

@1 = environment {
  bindings {}
}

@"Unsigned:+" = function ([unsigned(32)] %args, {} %kwargs) -> tvar(3) {
entry:
  return int 1
}

@"Unsigned:+.2" = function ([tvar(1)] %args, {} %kwargs) -> tvar(3) {
entry:
  return int 1
}

@main = function () -> nil {
entry:
  %x = tvar(2) call @"Unsigned:+" ([unsigned(32) 1, unsigned(32) 2], {})
  primitive "debug" (%x)
  return nil
}

map function @"Unsigned:+.2" => @"Unsigned:+"
