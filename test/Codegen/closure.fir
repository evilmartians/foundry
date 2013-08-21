; RUN: %foundry_gen %s | lli | %file_check %s

@env = environment {
  bindings {
    "x" = (0 0) mutable unsigned(32) 0
  }
}

@add = function (environment {"x" = (0 0) mutable unsigned(32)} %env,
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
  %adder.globl = closure (unsigned(32)) -> nil make_closure @add, @env
  jump %x
x:
  %adder.globl.1 = closure (unsigned(32)) -> nil phi [%entry=>%adder.globl]
  call @add_three (%adder.globl.1)

; CHECK: [DEBUG: 0x00000003]
  %result = unsigned(32) lvar_load @env, "x"
  primitive "debug" (%result)

  return unsigned(32) 0
}
