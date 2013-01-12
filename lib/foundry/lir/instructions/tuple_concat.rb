module Foundry
  class LIR::TupleConcatInsn < Furnace::SSA::Instruction
    def type
      CombinedTupleType.new(@operands.map(&:type))
    end
  end
end