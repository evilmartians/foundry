# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

class Fixed
  def +(other)
    invokeprimitive int_add(self, other)
  end
end

def optfun(a, b=1u32, c:, d: b + c)
  invokeprimitive debug(a)
  invokeprimitive debug(b)
  invokeprimitive debug(c)
  invokeprimitive debug(d)
end

def main
# CHECK: [DEBUG: 0x00000000]
# CHECK: [DEBUG: 0x00000001]
# CHECK: [DEBUG: 0x00000002]
# CHECK: [DEBUG: 0x00000003]
  self.optfun(0u32, c: 2u32)

# CHECK: [DEBUG: 0x00000010]
# CHECK: [DEBUG: 0x00000020]
# CHECK: [DEBUG: 0x00000030]
# CHECK: [DEBUG: 0x00000040]
  self.optfun(0x10u32, 0x20u32, c: 0x30u32, d: 0x40u32)
end
