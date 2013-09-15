@__stack_end__ = external global i32

declare void @_startup()
declare extern_weak void @_nmi()
declare extern_weak void @_hard_fault()
declare extern_weak void @_mpu_fault()
declare extern_weak void @_bus_fault()
declare extern_weak void @_usage_fault()
declare extern_weak void @_svcall()
declare extern_weak void @_debug_trap()
declare extern_weak void @_pendsv()
declare extern_weak void @_systick()

@__vectors__ = appending global [16 x i32*] [
  i32* @__stack_end__,
  i32* bitcast(void()* @_startup      to i32*),
  i32* bitcast(void()* @_nmi          to i32*),
  i32* bitcast(void()* @_hard_fault   to i32*),
  i32* bitcast(void()* @_mpu_fault    to i32*),
  i32* bitcast(void()* @_bus_fault    to i32*),
  i32* bitcast(void()* @_usage_fault  to i32*),
  i32* null, ; reserved
  i32* null, ; reserved
  i32* null, ; reserved
  i32* null, ; reserved
  i32* bitcast(void()* @_svcall       to i32*),
  i32* bitcast(void()* @_debug_trap   to i32*),
  i32* null, ; reserved
  i32* bitcast(void()* @_pendsv       to i32*),
  i32* bitcast(void()* @_systick      to i32*)
], section ".vectors"
