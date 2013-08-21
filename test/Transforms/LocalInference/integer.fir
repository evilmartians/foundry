; RUN: %foundry_opt %s -no-roots -infer | %file_check %s

; CHECK: @a = function () -> int {
@a = function () -> tvar(1) {
entry:
; CHECK: %x = int primitive "int_add"
  %x = tvar(2) primitive "int_add" (int 1, int 2)
  return %x
}
