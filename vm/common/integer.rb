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
    FoundryRt.intop :+, self, other
  end

  def -(other)
    FoundryRt.intop :-, self, other
  end

  def *(other)
    FoundryRt.intop :*, self, other
  end

  def /(other)
    FoundryRt.intop :/, self, other
  end

  def %(other)
    FoundryRt.intop :%, self, other
  end

  def <(other)
    FoundryRt.intop :<,  self, other
  end

  def <=(other)
    FoundryRt.intop :<=, self, other
  end

  def >(other)
    FoundryRt.intop :>,  self, other
  end

  def >=(other)
    FoundryRt.intop :>=, self, other
  end
end