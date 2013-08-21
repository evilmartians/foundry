; RUN: %foundry_gen %s | lli | %file_check %s

@add = function (environment {"x" = (0 0) mutable unsigned(32)} -> {} %env,
                 unsigned(32) %v) -> nil {
entry:
  %x     = unsigned(32) lvar_load %env, "x"
  %x.add = unsigned(32) primitive "int_add" (%x, %v)
  lvar_store %env, "x", %x.add
  return nil
}

@add_three = function (closure (unsigned(32)) -> nil %adder) -> nil {
entry:
  call_closure %adder (unsigned(32) 3)
  return nil
}

@main = function () -> unsigned(32) {
entry:
  %env = environment {"x" = (0 0) mutable unsigned(32)} -> {} frame empty
  lvar_store %env, "x", unsigned(32) 0

  %adder.globl = closure (unsigned(32)) -> nil make_closure @add, %env
  call @add_three (%adder.globl)

; CHECK: [DEBUG: 0x00000003]
  %result = unsigned(32) lvar_load %env, "x"
  primitive "debug" (%result)

  return unsigned(32) 0
}
