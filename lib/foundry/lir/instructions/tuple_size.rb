module Foundry
  class LIR::TupleSizeInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :tuple
    end

    def type
      Type.klass(VI::Machine_Integer)
    end
  end
end
