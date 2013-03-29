module Foundry
  class LIR::TupleInsn < Furnace::SSA::Instruction
    def type
      if operands
        Type::Tuple.new(operands.map(&:type))
      else
        Type.top
      end
    end
  end
end
