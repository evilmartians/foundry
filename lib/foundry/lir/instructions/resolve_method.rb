module Foundry
  class LIR::ResolveMethodInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :receiver
      s.operand :method#,   Type.klass(VI::Symbol)
    end

    def type
      LIR::Function.to_type
    end
  end
end
