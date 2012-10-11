module Kernel
  def nil?
    false
  end

  alias send __send__

  def trace(str)
    Foundry.primitive :trace, str
  end
end