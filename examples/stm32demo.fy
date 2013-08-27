  #################################
  #   LANGUAGE STANDARD LIBRARY   #
  #################################

class Value
  def self.new(*args, **kwargs)
    let instance = invokeprimitive obj_alloc(self)
    instance.initialize(*args, **kwargs)
  end
end

class Unsigned
  def <(other)
    invokeprimitive int_ult(self, other)
  end

  def +(other)
    invokeprimitive int_add(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
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

class Register(\alignment, \width) < Value
  def @address : Unsigned(\width)

  def initialize(address)
    @address = address
  end

  def value
    self.reg_read
  end

  def value=(new_value)
    self.reg_write(new_value)
  end

  # Internal functions to access the actual hardware register.
  def reg_read() : (Register(\alignment, \width)) -> Unsigned(\width)
    invokeprimitive mem_loadv(\alignment,  @address)
  end

  def reg_write(value) : (Register(\alignment, \width), Unsigned(\width)) -> Nil
    invokeprimitive mem_storev(\alignment, @address, value)
  end
end

  ################################
  #   STM32F1 STANDARD LIBRARY   #
  ################################

# ...

  ##########################
  #   USER-AUTHORED CODE   #
  ##########################

# RCC->APB2ENR |= IOPCEN; // 0x10
#  store 0x40021000 + 0x18, 0x10, 4
# GPIOC->CRH = 0x44444411;
#  store 0x40011000 + 0x04, 0x44444411, 4
# GPIOC->ODR = BIT(8);
#  store 0x40011000 + 0x0C, 0x100, 4

def delay(reg, val)
  let mut i = 0u32
  while i < 100000u32
    reg.value = val
    i += 1u32
  end
end

def main
  let apb2enr   = Register(4, 32).new(1073877016u32)
  apb2enr.value = 16u32
  let crh       = Register(4, 32).new(1073811460u32)
  crh.value     = 1145324561u32
  let odr       = Register(4, 32).new(1073811468u32)

  while true
    self.delay(odr, 256u32)
    self.delay(odr, 0u32)
  end
end
