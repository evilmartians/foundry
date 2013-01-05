module Foundry
  class LIR::InvokeMethodInsn < Furnace::SSA::GenericInstruction
    syntax do |s|
      s.operand :receiver
      s.operand :method,    VI::Symbol
      s.operand :arguments
      s.operand :block
    end
  end
end