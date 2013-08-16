; RUN: %foundry_as %s -o - | llvm-dis | %file_check %s

@main = function () -> type unsigned(32) {
; CHECK: entry:
; CHECK-NEXT: br label %foo
entry:
  jump %foo
; CHECK: foo:
; CHECK-NEXT: br i1 true, label %bar, label %baz
foo:
  jump_if true, %bar, %baz
bar:
  return unsigned(32) 0
baz:
  return unsigned(32) 0
}
