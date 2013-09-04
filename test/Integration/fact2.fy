# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

# CHECK: [DEBUG: 0x00375f00]

class Unsigned
  def *(other)
    invokeprimitive int_mul(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
  end

  def >(other)
    invokeprimitive int_ugt(self, other)
  end
end

class Lambda
  def call(*args, **kwargs)
    invokeprimitive lam_call(self, args, kwargs)
  end
end

def main
  let mut f1 = (x) { x }
  let f2     = (x) { if x > 1u32 then x * f1.call(x - 1u32) else 1u32 end }
  f1         = (x) { if x > 1u32 then x * f2.call(x - 1u32) else 1u32 end }
  invokeprimitive debug(f1.call(10u32))
end
