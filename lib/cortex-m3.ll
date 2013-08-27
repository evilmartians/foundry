@__stack_end__ = external global i32
declare void @_startup()

@__vectors__ = global [2 x i32*] [
  i32* @__stack_end__,
  i32* bitcast(void()* @_startup to i32*)
], section ".vectors"
