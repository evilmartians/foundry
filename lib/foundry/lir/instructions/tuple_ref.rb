module Foundry
  class LIR::TupleRefInsn < Furnace::SSA::Instruction
    attr_accessor :index

    syntax do |s|
      s.operand :tuple, Monotype.of(VI::Tuple)
    end

    def initialize(basic_block, index, operands=[], name=nil)
      @index = index.to_i

      super(basic_block, operands, name)
    end

    def pretty_parameters(p=LIR::PrettyPrinter.new)
      p.text @index, ','
    end

    def type
      tuple_ty = tuple.type

      tuple_ty.is_a?(TupleType) &&
          tuple_ty.size &&
          tuple_ty.element_types[@index]
    end
  end
end