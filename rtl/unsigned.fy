class Unsigned
  def +(other)
    invokeprimitive int_add(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
  end

  def *(other)
    invokeprimitive int_mul(self, other)
  end

  def /(other)
    invokeprimitive int_udiv(self, other)
  end

  # def %(other)
  #   invokeprimitive int_mod(self, other)
  # end

  def **(power)
    invokeprimitive int_exp(self, power)
  end

  def -@()
    0u32 - self
  end

  def &(other)
    invokeprimitive int_and(self, other)
  end

  def |(other)
    invokeprimitive int_or(self, other)
  end

  def ^(other)
    invokeprimitive int_xor(self, other)
  end

  def >>(bits)
    invokeprimitive int_lshr(self, bits)
  end

  def <<(bits)
    invokeprimitive int_shl(self, bits)
  end

  def ~@()
    -self - 1u32
  end

  def ==(other)
    invokeprimitive int_eq(self, other)
  end

  def !=(other)
    invokeprimitive int_ne(self, other)
  end

  def >(other)
    invokeprimitive int_ugt(self, other)
  end

  def >=(other)
    invokeprimitive int_uge(self, other)
  end

  def <(other)
    invokeprimitive int_ult(self, other)
  end

  def <=(other)
    invokeprimitive int_ule(self, other)
  end

  def times(block)
    let mut i = 0u32
    while i < self
      block.call(i)
      i += 1u32
    end
  end
end
