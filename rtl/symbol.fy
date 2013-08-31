class Symbol
  def to_s
    invokeprimitive sym_to_str(self)
  end
end
