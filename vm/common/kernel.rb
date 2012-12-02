module Kernel
  def nil?
    false
  end

  alias send __send__

  def equal?(other)
    FoundryRt.equal? self, other
  end

  alias eql? equal?
  alias ==   equal?
  alias ===  equal?

  def class
    FoundryRt.class_of self
  end

  def singleton_class
    FoundryRt.singleton_class_of self
  end

  def is_a?(klass)
    FoundryRt.is_a? self, klass
  end

  alias kind_of? is_a?

  def instance_of?(klass)
    self.class == klass
  end

  def instance_variables
    FoundryRt.ilist(self)
  end

  def instance_variable_get(name)
    FoundryRt.ivar(self, name.to_sym)
  end

  def instance_variable_set(name, value)
    FoundryRt.iasgn(self, name.to_sym, value)
  end

  def to_s
    "#<#{self.class}>"
  end

  def inspect
    to_s
  end

  def proc(&prc)
    raise ArgumentError, "block required" if prc.nil?
    prc
  end

  def lambda(&prc)
    raise ArgumentError, "block required" if prc.nil?
    prc.lambda_style!
    prc
  end

  def trace(obj)
    FoundryRt.trace obj
  end
end