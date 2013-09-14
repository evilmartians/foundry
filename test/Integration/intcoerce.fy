# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

class Fixed
  def +(other)
    invokeprimitive int_add(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
  end

  def <<(other)
    invokeprimitive int_shl(self, other)
  end

  def -@
    0 - self
  end

  def ~@
    -self - 1
  end
end

def main
# CHECK: [DEBUG: 0x0000000b]
  invokeprimitive debug(1u32 + 10)
# CHECK: [DEBUG: 0x0000000c]
  invokeprimitive debug(10 + 2u32)

# CHECK: [DEBUG: 0x00000023]
  let x = 10 + 20
  invokeprimitive debug(x + 5u32)

# CHECK: [DEBUG: 0xfffffdff]
  invokeprimitive debug(~0x200u32)

# CHECK: [DEBUG: 0xfffffffc]
  invokeprimitive debug(~(1 << 2) + 1u32)
end
