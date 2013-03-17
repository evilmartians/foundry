module Foundry
  class LIR::TupleConcatInsn < Furnace::SSA::Instruction
    def type
      if operands.map(&:type).any?(&:variable?)
        Type.top
      else
        Type::Tuple.new(
            operands.
              map(&:type).
              map(&:element_types).
              reduce(&:+))
      end
    end
  end
end
