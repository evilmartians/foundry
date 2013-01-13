class Symbol
  def self.coerce(object)
    object.to_sym
  end

  def to_sym
    self
  end

  def to_s
    FoundryRt.symop(:to_s, self)
  end
end