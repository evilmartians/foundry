; RUN: %foundry_gen %s | lli | %file_check %s
; CHECK: [DEBUG: 0x00000001]
; CHECK: [DEBUG: 0x00000002]

@a = function () -> nil {
entry:
  primitive "debug" (unsigned(32) 1)
  return nil
}

@b = function () -> nil {
entry:
  primitive "debug" (unsigned(32) 2)
  return nil
}

@main = function () -> unsigned(32) {
do_a:
  jump %print
do_b:
  jump %print
print:
  %fun  = function () -> nil phi [ %do_a => @a, %do_b => @b ]
  %next = boolean phi [ %do_a => true, %do_b => false ]
  call %fun ()
  jump_if %next, %do_b, %exit
exit:
  return unsigned(32) 0
}
