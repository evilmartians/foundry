# RUN: %not %foundry_vm %s 2>%t
# RUN: %file_check %s -input-file %t

# CHECK: d0001_lvar_undeclared.fy:[[@LINE+1]]:1: error: Local variable `foo' is not declared.
foo
