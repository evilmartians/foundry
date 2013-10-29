; RUN: %foundry_xfrm %s | %file_check %s

; CHECK: @1 = function "" () -> nil
@"" = function "" () -> nil {
entry:
  return nil
}
