module Foundry
  class LIR::TraceInsn < Furnace::SSA::Instruction
    def has_side_effects?
      true
    end
  end
end