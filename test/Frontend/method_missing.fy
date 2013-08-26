# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

# CHECK: [DEBUG: 0x00000001]

class A
  def self.new
    invokeprimitive obj_alloc(self)
  end

  def method_missing(*args)
    invokeprimitive debug(1u32)
  end
end

def main
  let a = A.new
  a.foo
end
