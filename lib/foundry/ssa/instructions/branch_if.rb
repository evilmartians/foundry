module Foundry
  class SSA::BranchIfInsn < Furnace::SSA::TerminatorInstruction
    syntax do |s|
      s.operand :condition
      s.operand :true_target,  SSA::BasicBlock
      s.operand :false_target, SSA::BasicBlock
    end
  end
end