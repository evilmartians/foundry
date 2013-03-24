module Foundry
  class LIR::ClosureInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :binding
      s.operand :callee
    end

    def type
      Type.top
    end
  end
end
