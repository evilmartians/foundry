; RUN:   %foundry <%s - | llvm-dis | %file_check %s
; CHECK: define void @main

@main = function () -> type nil {
entry:
  return nil
}
