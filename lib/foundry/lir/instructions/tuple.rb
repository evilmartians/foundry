module Foundry
  class LIR::TupleInsn < Furnace::SSA::Instruction
    def type
      TupleType.new(@operands)
    end
  end
end