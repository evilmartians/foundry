; RUN: %foundry_opt %s -no-roots -simplify-cfg | %file_check %s

@foo = function() -> nil {
; CHECK: entry:
; CHECK-NEXT: return nil
entry:
  jump %foo
foo:
  return nil
; CHECK-NOT: bar:
bar:
  return nil
}
