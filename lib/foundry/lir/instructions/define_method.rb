module Foundry
  class LIR::DefineMethodInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass
      s.operand :method,  VI::Symbol
      s.operand :body,    VI::Proc
    end

    def has_side_effects?
      true
    end
  end
end