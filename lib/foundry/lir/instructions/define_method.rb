module Foundry
  class LIR::DefineMethodInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass
      s.operand :method,  Monotype.of(VI::Symbol)
      s.operand :body,    Monotype.of(VI::Proc)
    end

    def has_side_effects?
      true
    end
  end
end