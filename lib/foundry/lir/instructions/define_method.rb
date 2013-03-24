module Foundry
  class LIR::DefineMethodInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass
      s.operand :method
      s.operand :body
    end

    def has_side_effects?
      true
    end
  end
end
