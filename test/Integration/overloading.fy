# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms | %foundry_gen -o %t2
# RUN: llvm-dis %t2 -o - | %file_check %s

# CHECK: Fixed

class Fixed
  def +(other)
    invokeprimitive int_add(self, other)
  end
end

def main
  1u32 + 1u32
  1u16 + 1u16
end
