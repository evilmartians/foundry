# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms -o %t2
# RUN: %foundry_gen  %t2 | lli | %file_check %s

# CHECK: [DEBUG: 0x00000019]

class Unsigned
  def +(other)
    invokeprimitive int_add(self, other)
  end

  def *(other)
    invokeprimitive int_mul(self, other)
  end
end

class Vector1D < Value
  def @x : Unsigned(32i)
end

class Vector2D < Vector1D
  def @y : Unsigned(32i)

  def self.new(*args)
    let instance = invokeprimitive obj_alloc(self)
    instance.initialize(*args)
  end

  def initialize(x, y)
    @x = x
    @y = y
  end

  def length_sq
    @x * @x + @y * @y
  end
end

def main
  let x = Vector2D.new(3u32, 4u32)
  invokeprimitive debug(x.length_sq)
end
