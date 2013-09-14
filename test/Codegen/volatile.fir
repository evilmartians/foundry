; RUN: %foundry_gen %s -o - | llvm-dis | %file_check %s

@main = function () -> nil {
entry:
; CHECK: %x = load i32* inttoptr (i32 1024 to i32*), align 4
  %x = unsigned(32) primitive "mem_load" (unsigned(32) 4, unsigned(32) 1024)
; CHECK: %y = load volatile i32* inttoptr (i32 1024 to i32*), align 4
  %y = unsigned(32) primitive "mem_loadv" (unsigned(32) 4, unsigned(32) 1024)
; CHECK: store i32 16, i32* inttoptr (i32 1024 to i32*), align 4
  primitive "mem_store" (unsigned(32) 4, unsigned(32) 1024, unsigned(32) 16)
; CHECK: store volatile i32 16, i32* inttoptr (i32 1024 to i32*), align 4
  primitive "mem_storev" (unsigned(32) 4, unsigned(32) 1024, unsigned(32) 16)
  return nil
}
