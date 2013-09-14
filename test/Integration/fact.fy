# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -[ -worklist -resolve -specialize -infer -] -gdce -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

# CHECK: [DEBUG: 0x00375f00]

class Fixed
  def ==(other)
    invokeprimitive int_eq(self, other)
  end

  def *(other)
    invokeprimitive int_mul(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
  end
end

def fact(n)
  if n == 1u32
    n
  else
    n * self.fact(n - 1u32)
  end
end

def main
  invokeprimitive debug(self.fact(10u32));
  nil
end
