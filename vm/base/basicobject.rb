class BasicObject
  def initialize
  end
  private :initialize

  def equal?(other)
    Foundry.primitive :equal?
  end

  alias == equal?

  def !
    self.equal?(false) || self.equal?(nil) ? true : false
  end

  def !=(other)
    (self == other) ? false : true
  end

  def singleton_method_added(name)
  end
  private :singleton_method_added

  def singleton_method_removed(name)
  end
  private :singleton_method_removed

  def singleton_method_undefined(name)
  end
  private :singleton_method_undefined
end