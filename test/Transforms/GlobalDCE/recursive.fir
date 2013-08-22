; RUN: %foundry_opt %s -no-roots -gdce | %file_check %s

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
