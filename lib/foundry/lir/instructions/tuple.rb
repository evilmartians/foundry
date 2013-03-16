module Foundry
  class LIR::TupleInsn < Furnace::SSA::Instruction
    def type
      Type::Tuple.new(@operands.map(&:type))
    end
  end
end
