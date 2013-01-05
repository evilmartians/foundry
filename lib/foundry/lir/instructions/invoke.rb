module Foundry
  class LIR::InvokeInsn < Furnace::SSA::GenericInstruction
    syntax do |s|
      s.operand :function,  LIR::Function
      s.splat   :arguments
    end

    def has_side_effects?
      true
    end
  end
end