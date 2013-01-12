module Foundry
  class LIR::AllocateInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass, Monotype.of(VI::Class)
    end

    def type
      if klass.constant?
        Monotype.of(klass.value)
      end
    end
  end
end