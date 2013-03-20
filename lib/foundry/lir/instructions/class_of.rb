module Foundry
  class LIR::ClassOfInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :object
    end

    def type
      Type.top
    end
  end
end
