class Symbol
  def self.coerce(object)
    object.to_sym
  end

  def to_sym
    self
  end
end