; RUN: %foundry_xfrm %s -no-roots -infer | %file_check %s

; CHECK: @a = function "a" () -> nil {
@a = function () -> tvar(1) {
entry:
  jump %loop
loop:
  jump %loop
}
