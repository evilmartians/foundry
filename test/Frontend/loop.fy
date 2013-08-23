# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -[ -worklist -resolve -specialize -infer -] -gdce -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

# CHECK:     [DEBUG: 0x00000000]
# CHECK:     [DEBUG: 0x00000001]
# CHECK:     [DEBUG: 0x00000002]
# CHECK:     [DEBUG: 0x00000003]
# CHECK:     [DEBUG: 0x00000004]
# CHECK-NOT: [DEBUG:

class Unsigned
  def <(other)
    invokeprimitive int_ult(self, other)
  end

  def +(other)
    invokeprimitive int_add(self, other)
  end
end

def main
  let mut x = 0u32
  while x < 5u32
    invokeprimitive debug(x)
    x = x + 1u32
  end
end
