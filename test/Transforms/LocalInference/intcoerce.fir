; RUN: %foundry_xfrm %s -no-roots -infer | %file_check %s

@b = function (unsigned(32) %x) -> nil {
entry:
  primitive "debug"(%x)
  return nil
}

; CHECK: @a = function () -> nil {
@a = function () -> nil {
entry:
  %x = tvar(1) primitive "int_coerce" (int 10)
; CHECK: call @b (unsigned(32) 10)
  call @b(%x)
  return nil
}

map tvar = 2
