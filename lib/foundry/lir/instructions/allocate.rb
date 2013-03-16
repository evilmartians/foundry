module Foundry
  class LIR::AllocateInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass, Type.klass(VI::Class)
    end

    def type
      Type.klass(klass)
    end
  end
end
