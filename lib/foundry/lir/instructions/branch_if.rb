module Foundry
  class LIR::BranchIfInsn < Furnace::SSA::TerminatorInstruction
    syntax do |s|
      s.operand :condition
      s.operand :true_target,  LIR::BasicBlock
      s.operand :false_target, LIR::BasicBlock
    end

    def exits?
      false
    end
  end
end