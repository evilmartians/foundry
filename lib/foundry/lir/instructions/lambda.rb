module Foundry
  class LIR::LambdaInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :binding, VI::Binding
      s.operand :code,    LIR::Function
    end

    def type
      VI::Proc
    end
  end
end