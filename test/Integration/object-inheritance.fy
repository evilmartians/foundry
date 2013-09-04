# RUN: %foundry_vm   %s -o %t1
# RUN: %foundry_xfrm %t1 -std-xfrms | %foundry_gen -o %t2
# RUN: llvm-dis %t2 -o - | %file_check %s -check-prefix CHECK-LL
# RUN: lli %t2 | %file_check %s

# CHECK: [DEBUG: 0x00000009]
# CHECK: [DEBUG: 0x00000019]

# CHECK-LL-DAG: %Vector1D = type { %Object, i32 }
# CHECK-LL-DAG: %Vector2D = type { %Vector1D, i32 }

class Unsigned
  def +(other)
    invokeprimitive int_add(self, other)
  end

  def *(other)
    invokeprimitive int_mul(self, other)
  end
end

class Object
  def self.new(*args)
    let instance = invokeprimitive obj_alloc(self)
    instance.initialize(*args)
    instance
  end
end

class Vector1D
  def @x : Unsigned(32)

  def initialize(x)
    @x = x
  end

  def x
    @x
  end

  def length_sq
    self.x * self.x
  end
end

class Vector2D < Vector1D
  def @y : Unsigned(32)

  def initialize(x, y)
    @x = x
    @y = y
  end

  def y
    @y
  end

  def length_sq
    self.x * self.x + self.y * self.y
  end
end

def main
  let a = Vector1D.new(3u32)
  invokeprimitive debug(a.length_sq)

  let b = Vector2D.new(3u32, 4u32)
  invokeprimitive debug(b.length_sq)
end
