module Foundry
  class LIR::InvokeClosureInsn < Furnace::SSA::GenericInstruction
    syntax do |s|
      s.operand :closure,   VI::Proc
      s.operand :arguments
      s.operand :block
    end
  end
end