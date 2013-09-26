; RUN: %foundry_xfrm %s -no-roots -simplify-cfg | %file_check %s

@foo = function () -> nil {
entry:
  jump_if true, %a, %b

a:
  jump %exit

b:
  jump %c

c:
  jump %exit

exit:
; CHECK: %0 = int phi [%a => int 1, %b => int 2]
  %0 = int phi [%a => int 1, %c => int 2]
  return nil
}
