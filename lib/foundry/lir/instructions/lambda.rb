module Foundry
  class LIR::LambdaInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :binding
      s.operand :code,    LIR::Function
    end

    def type
      VI::Proc
    end
  end
end