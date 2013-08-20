; RUN: %foundry_opt %s -no-roots -sccp | %file_check %s

@foo = function () -> nil {
entry:
  jump %loop
loop:
  %x     = int phi [ %entry => int 1 ], [ %incr => %x.add ]
  %cond  = boolean primitive "int_uge" (%x, int 5)
  jump_if %cond, %incr, %exit
incr:
  %x.add = int primitive "int_add" (%x, int 1)
  jump %loop
exit:
; CHECK: return int 1
  return %x
}
