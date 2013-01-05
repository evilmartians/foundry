class Integer < Numeric
  def self.coerce(object)
    object.to_int
  end

  def to_int
    self
  end

  alias to_i to_int

  def +@
    self
  end

  def -@
    0 - self
  end

  def +(other)
    FoundryRt.int_add self, other
  end

  def -(other)
    FoundryRt.int_sub self, other
  end

  def *(other)
    FoundryRt.int_mul self, other
  end

  def /(other)
    FoundryRt.int_div self, other
  end

  def %(other)
    FoundryRt.int_mod self, other
  end

  def <(other)
    FoundryRt.int_lt self, other
  end

  def <=(other)
    FoundryRt.int_lte self, other
  end

  def >(other)
    FoundryRt.int_gt self, other
  end

  def >=(other)
    FoundryRt.int_gte self, other
  end
end