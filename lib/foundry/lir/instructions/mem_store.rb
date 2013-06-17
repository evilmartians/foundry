module Foundry
  class LIR::MemStoreInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :address
      s.operand :value
      s.operand :alignment
    end

    def has_side_effects?
      true
    end
  end
end
