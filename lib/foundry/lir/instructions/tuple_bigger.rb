module Foundry
  class LIR::TupleBiggerInsn < Furnace::SSA::Instruction
    attr_accessor :min_size

    syntax do |s|
      s.operand :tuple, Monotype.of(VI::Tuple)
    end

    def initialize(basic_block, min_size, operands=[], name=nil)
      @min_size = min_size.to_i

      super(basic_block, operands, name)
    end

    def pretty_parameters(p)
      p.text @min_size, ','
    end

    def type
      tuple_ty = tuple.type

      tuple_ty.is_a?(TupleType) &&
          tuple_ty.size &&
          (tuple_ty.size > @min_size ?
              Foundry.typeof(VI::TRUE) :
              Foundry.typeof(VI::FALSE))
    end
  end
end