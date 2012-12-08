class BasicObject
  def initialize
  end
  private :initialize

  def equal?(other)
    FoundryRt.equal? self, other
  end

  alias == equal?

  def __id__
    ::Kernel.raise ::NotImplementedError, "Foundry does not implement object_id"
  end

  def __send__(Symbol method, *args, &block)
    FoundryRt.call self, method, args, block
  end

  def !
    equal?(false) || equal?(nil) ? true : false
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