; RUN: %foundry <%s - | llvm-dis | %file_check %s

@main = function () -> type nil {
; CHECK: entry:
; CHECK-NEXT: br label %foo
entry:
  jump %foo
; CHECK: foo:
; CHECK-NEXT: br i1 true, label %bar, label %baz
foo:
  jump_if true, %bar, %baz
bar:
  return nil
baz:
  return nil
}
