; RUN: %foundry_gen %s -o - | llvm-dis | %file_check %s

@main = function (boolean %cond) -> unsigned(32) {
entry:
; CHECK: %x = select i1 %cond, i32 0, i32 1
  %x = unsigned(32) select %cond, unsigned(32) 0, unsigned(32) 1
  return %x
}
