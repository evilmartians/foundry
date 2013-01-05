module Foundry
  class LIR::ResolveClosureInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :proc, VI::Proc
    end

    def type
      LIR::Function
    end
  end
end