module Foundry
  class SSA::CallInsn < Furnace::SSA::GenericInstruction
    syntax do |s|
      s.operand :closure
      s.operand :self
      s.operand :arguments
      s.operand :block
    end
  end
end