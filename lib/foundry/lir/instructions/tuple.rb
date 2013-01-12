module Foundry
  class LIR::TupleInsn < Furnace::SSA::Instruction
    def type
      LiteralTupleType.new(@operands)
    end
  end
end