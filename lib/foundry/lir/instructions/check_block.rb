module Foundry
  class LIR::CheckBlockInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :block, VI::Proc
    end
  end
end