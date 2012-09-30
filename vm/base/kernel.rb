module Kernel
  def nil?
    false
  end

  alias send __send__

  def trace(str)
    Foundry.primitive :trace
  end
end