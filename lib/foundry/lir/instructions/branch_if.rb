module Foundry
  class LIR::BranchIfInsn < Furnace::SSA::TerminatorInstruction
    syntax do |s|
      s.operand :condition
      s.operand :true_target
      s.operand :false_target
    end

    def exits?
      false
    end
  end
end
