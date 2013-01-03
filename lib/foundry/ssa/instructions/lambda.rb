module Foundry
  class SSA::LambdaInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :binding,  VI::Binding
      s.operand :function, SSA::Function
    end

    def type
      VI::Proc
    end
  end
end