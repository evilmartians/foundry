# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms 2>&1 | %file_check %s

# CHECK-NOT: Convergence terminated

class Unsigned
  def +(other)
    invokeprimitive int_add(self, other)
  end
end

def main
  1u32 + 1u32
  1u16 + 1u16
end
