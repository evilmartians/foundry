module Foundry
  class LIR::DefineMethodInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass
      s.operand :method,  Type.klass(VI::Symbol)
      s.operand :body,    Type.klass(VI::Proc)
    end

    def has_side_effects?
      true
    end
  end
end
