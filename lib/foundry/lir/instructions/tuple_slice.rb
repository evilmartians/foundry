module Foundry
  class LIR::TupleSliceInsn < Furnace::SSA::Instruction
    attr_accessor :from, :to

    syntax do |s|
      s.operand :tuple, Monotype.of(VI::Tuple)
    end

    def initialize(basic_block, from, to, operands=[], name=nil)
      @from, @to = from.to_i, to.to_i

      super(basic_block, operands, name)
    end

    def pretty_parameters(p)
      p.text @from, '..', @to, ','
    end

    def type
      tuple_ty = tuple.type

      tuple_ty.is_a?(TupleType) &&
          tuple_ty.elements &&
          TupleType.new(tuple_ty.elements[@from..@to])
    end
  end
end