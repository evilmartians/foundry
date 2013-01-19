class String
  def to_str
    self
  end

  alias to_s to_str

  def to_sym
    FoundryRt.strop(:to_sym, self)
  end

  def +(other)
    # FIXME
    FoundryRt.strop(:+, self, other)
  end
end