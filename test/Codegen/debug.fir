; RUN: %foundry_gen %s -o - | lli | %file_check %s

; CHECK: [DEBUG: 0x00000042]
@main = function () -> unsigned(32) {
entry:
  primitive "debug" (unsigned(32) 66)
  return unsigned(32) 0
}
