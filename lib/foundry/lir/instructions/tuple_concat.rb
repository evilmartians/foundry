module Foundry
  class LIR::TupleConcatInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :left_tuple,  Monotype.of(VI::Tuple)
      s.operand :right_tuple, Monotype.of(VI::Tuple)
      s.splat   :elements
    end

    def type
      CombinedTupleType.new([
          left_tuple.type,
          right_tuple.type,
          LiteralTupleType.new(elements)
      ])
    end
  end
end