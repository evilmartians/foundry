module Kernel
  def nil?
    equal? nil
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

  def is_a?(Module mod)
    FoundryRt.is_a? self, mod
  end

  alias kind_of? is_a?

  def instance_of?(Module mod)
    self.class == mod
  end

  def instance_variables
    FoundryRt.ivlist(self)
  end

  def instance_variable_get(Symbol name)
    FoundryRt.ivar(self, name)
  end

  def instance_variable_set(Symbol name, value)
    FoundryRt.iasgn(self, name, value)
  end

  def to_s
    "#<#{self.class.name}>"
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