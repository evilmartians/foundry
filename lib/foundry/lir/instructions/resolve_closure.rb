module Foundry
  class LIR::ResolveClosureInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :closure
    end

    def type
      LIR::Function.to_type
    end
  end
end
