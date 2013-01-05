module Foundry
  class LIR::IvarStoreInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :object
      s.operand :variable, VI::Symbol
      s.operand :value
    end

    def has_side_effects?
      true
    end
  end
end