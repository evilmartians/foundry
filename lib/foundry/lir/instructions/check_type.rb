module Foundry
  class LIR::CheckTypeInsn < Furnace::SSA::GenericInstruction
    syntax do |s|
      s.operand :expected_type
      s.operand :expression
    end
  end
end
