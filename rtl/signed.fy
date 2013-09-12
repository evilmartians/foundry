class Signed
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
    invokeprimitive int_sdiv(self, other)
  end

  # def %(other)
  #   invokeprimitive int_mod(self, other)
  # end

  def **(power)
    invokeprimitive int_exp(self, power)
  end

  def -@()
    0s64 - self
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
    invokeprimitive int_ashr(self, bits)
  end

  def <<(bits)
    invokeprimitive int_shl(self, bits)
  end

  def ~@()
    -self - 1
  end

  def ==(other)
    invokeprimitive int_eq(self, other)
  end

  def !=(other)
    invokeprimitive int_ne(self, other)
  end

  def >(other)
    invokeprimitive int_sgt(self, other)
  end

  def >=(other)
    invokeprimitive int_sge(self, other)
  end

  def <(other)
    invokeprimitive int_slt(self, other)
  end

  def <=(other)
    invokeprimitive int_sle(self, other)
  end
end
