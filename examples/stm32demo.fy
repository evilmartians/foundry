  #################################
  #   LANGUAGE STANDARD LIBRARY   #
  #################################

class Class
  def define_method(name, body)
    invokeprimitive cls_defm (self, name, body)
  end
end

class Value
  def self.new(*args, **kwargs)
    let instance = invokeprimitive obj_alloc(self)
    instance.initialize(*args, **kwargs)
  end

  def ==(other)
    invokeprimitive obj_equal(self, other)
  end

  def !=(other)
    not self == other
  end
end

class Symbol
  def to_s
    invokeprimitive sym_to_str(self)
  end
end

class Unsigned
  def ==(other)
    invokeprimitive int_eq(self, other)
  end

  def !=(other)
    invokeprimitive int_ne(self, other)
  end

  def <(other)
    invokeprimitive int_ult(self, other)
  end

  def +(other)
    invokeprimitive int_add(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
  end

  def -@
    0u32 - self
  end

  def ~@
    -self - 1u32
  end

  def |(other)
    invokeprimitive int_or(self, other)
  end

  def &(other)
    invokeprimitive int_and(self, other)
  end

  def <<(other)
    invokeprimitive int_shl(self, other)
  end

  def >>(other)
    invokeprimitive int_lshr(self, other)
  end
end

class Lambda
  def call(*args, **kwargs)
    invokeprimitive lam_call(args, kwargs)
  end
end

  #################################
  #   EMBEDDED STANDARD LIBRARY   #
  #################################

class Register(\width) < Value
  def @value : Unsigned(\width)

  def initialize(value)
    @value = value
  end

  def value
    @value
  end

  def self.flag(name, kind, offset)
    let mask = 1u32 << offset

    if kind == :r or kind == :rw
      self.define_method(name, (self) do
        @value & mask != 0u32
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (value, field) do
        value & ~mask | (if field then mask else 0u32 end)
      end)
    end
  end
end

class Unit < Value
  def @base : Unsigned(32)

  def initialize(base)
    @base = base
  end

  def self.register(name, cls, kind, offset, align: 4)
    if kind == :r or kind == :rw
      self.define_method(name, (self) do
        cls.new(invokeprimitive mem_loadv(align, @base + offset))
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"#{name}=", (self, reg) do
        invokeprimitive mem_storev(align, @base + offset, reg.value)
      end)
    end
  end
end

  ################################
  #   STM32F1 STANDARD LIBRARY   #
  ################################

class APB2ENR < Register(32)
  self.flag(:iopcen, :rw, 4u32)

  def |(other)
    let mut value = @value
    value = self.set_iopcen(value, other.iopcen)
    self { @value = value }
  end
end

class RCCUnit < Unit
  self.register(:APB2ENR, APB2ENR, :rw, 0x18_u32, align: 4)
end

RCC = RCCUnit.new(0x4002_1000_u32)

  ##########################
  #   USER-AUTHORED CODE   #
  ##########################

# RCC->APB2ENR |= IOPCEN; // 0x10
#  store 0x40021000 + 0x18, 0x10, 4
# GPIOC->CRH = 0x44444411;
#  store 0x40011000 + 0x04, 0x44444411, 4
# GPIOC->ODR = BIT(8);
#  store 0x40011000 + 0x0C, 0x100, 4

def main
  RCC.APB2ENR |= { iopcen: true }
end
