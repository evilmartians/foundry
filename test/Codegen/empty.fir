; RUN: %foundry_as %s -o - | llvm-dis | %file_check %s

; CHECK: define i32 @main()
@main = function () -> unsigned(32) {
; CHECK: entry:
; CHECK-NEXT: ret i32 0
entry:
  return unsigned(32) 0
}
