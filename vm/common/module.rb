class Module
  def name # attr_reader
    @name
  end

  def public(*)
    # TODO
  end

  def private(*)
    # TODO
  end

  def protected(*)
    # TODO
  end

  private :public, :private, :protected

  def include(modulus)
    FoundryRt.include self, modulus
  end
end