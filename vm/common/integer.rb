class Integer
  def +@
    self
  end

  def -@
    0 - self
  end

  def +(other)
    Foundry.primitive :int_add, self, other
  end

  def -(other)
    Foundry.primitive :int_sub, self, other
  end

  def *(other)
    Foundry.primitive :int_mul, self, other
  end

  def /(other)
    Foundry.primitive :int_div, self, other
  end

  def <(other)
    Foundry.primitive :int_lt, self, other
  end

  def <=(other)
    Foundry.primitive :int_lte, self, other
  end

  def >(other)
    Foundry.primitive :int_gt, self, other
  end

  def >=(other)
    Foundry.primitive :int_gte, self, other
  end

  def times
    i = 0

    while i < self
      yield i
      i += 1
    end

    self
  end
end