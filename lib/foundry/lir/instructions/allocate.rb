module Foundry
  class LIR::AllocateInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass
    end

    def type
      if klass.constant?
        Type.klass klass.value
      else
        Type.top
      end
    end
  end
end
