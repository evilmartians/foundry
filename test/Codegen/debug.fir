; RUN: %foundry_as %s -o - | lli | %file_check %s

; CHECK: [DEBUG: 0x00000042]
@main = function () -> type unsigned(32) {
entry:
  type nil primitive "debug" (unsigned(32) 66)
  return unsigned(32) 0
}
