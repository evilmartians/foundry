; RUN: %foundry_xfrm %s -no-roots -gdce | %file_check %s

; CHECK: @foo

@main = function () -> nil {
entry:
  jump %next
next:
  %foo = function () -> nil phi [%entry => @foo]
  call %foo ()
  return nil
}

@foo = function () -> nil {
entry:
  return nil
}
