module Foundry
  class LIR::TupleBiggerInsn < Furnace::SSA::Instruction
    attr_accessor :size

    syntax do |s|
      s.operand :tuple, Monotype.of(VI::Tuple)
    end

    def initialize(basic_block, size, operands=[], name=nil)
      super(basic_block, operands, name)
      @size = size
    end

    def pretty_parameters(p)
      p.text @size, ','
    end

    def type
      tuple_ty = tuple.type

      tuple_ty.is_a?(TupleType) &&
          tuple_ty.size &&
          (tuple_ty.size > @size ?
              Foundry.typeof(VI::TRUE) :
              Foundry.typeof(VI::FALSE))
    end
  end
end