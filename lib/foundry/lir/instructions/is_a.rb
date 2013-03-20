module Foundry
  class LIR::IsAInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :object
      s.operand :klass
    end

    def type
      Type.boolean
    end
  end
end
