class Integer
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
    invokeprimitive int_div(self, other)
  end

  def %(other)
    invokeprimitive int_mod(self, other)
  end

  def **(power)
    invokeprimitive int_exp(self, power)
  end

  def -@()
    0 - self
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
    invokeprimitive int_shr(self, bits)
  end

  def <<(bits)
    invokeprimitive int_shl(self, bits)
  end

  def ~@()
    -self - 1
  end
end
