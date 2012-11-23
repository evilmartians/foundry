module Kernel
  def nil?
    false
  end

  alias send __send__

  def equal?(other)
    Foundry.primitive :equal?, self, other
  end

  alias eql? equal?
  alias ==   equal?
  alias ===  equal?

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
    Foundry.primitive :trace, obj
  end
end