; RUN: %foundry_opt %s -no-roots -infer | %file_check %s

; CHECK: @a = function ([int] %x) -> int
@a = function ([tvar(1)] %x) -> tvar(1) {
entry:
  return int 1
}
