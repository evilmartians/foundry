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