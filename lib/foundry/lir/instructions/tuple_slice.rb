module Foundry
  class LIR::TupleSliceInsn < Furnace::SSA::Instruction
    attr_accessor :from, :to

    syntax do |s|
      s.operand :tuple, Monotype.of(VI::Tuple)
    end

    def initialize(basic_block, from, to, operands=[], name=nil)
      super(basic_block, operands, name)
      @from, @to = from, to
    end

    def pretty_parameters(p)
      p.text @from, '..', @to, ','
    end

    def type
      tuple_ty = tuple.type

      tuple_ty.is_a?(TupleType) &&
          tuple_ty.size &&
          TupleType.new(tuple_ty.element_types[@from..@to])
    end
  end
end