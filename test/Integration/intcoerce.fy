# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

# CHECK: [DEBUG: 0x0000000a]

class Fixed
  def +(other)
    invokeprimitive int_add(self, other)
  end
end

def main
  invokeprimitive debug(0u32 + 10)
end
