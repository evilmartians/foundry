module Foundry
  class LIR::ClosureInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :binding, Monotype.of(VI::Binding)
      s.operand :code,    LIR::Function
    end

    def type
      Monotype.of(VI::Proc)
    end
  end
end