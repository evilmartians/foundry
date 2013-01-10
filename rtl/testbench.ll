declare void @printf(i8*, ...) nounwind

@trace_fmt = constant [ 13 x i8 ] c"trace: %llu\0a\00"

define void @foundry.trace(i64 %value) noinline {
  call void (i8*, ...)* @printf(i8* bitcast([ 13 x i8 ]* @trace_fmt to i8*), i64 %value)

  ret void
}