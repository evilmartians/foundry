; RUN: %foundry_xfrm %s -no-roots -gdce | %file_check %s

; CHECK-NOT: @foo

@main = function () -> nil {
entry:
  return nil
}

@foo = function () -> nil {
entry:
  call @foo ()
  return nil
}
