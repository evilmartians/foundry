class Module
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
    Foundry.primitive :include, self, modulus
  end
end