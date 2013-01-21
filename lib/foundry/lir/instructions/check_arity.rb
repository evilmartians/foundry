module Foundry
  class LIR::CheckArityInsn < Furnace::SSA::Instruction
    attr_accessor :min, :max

    syntax do |s|
      s.operand :arguments, Monotype.of(VI::Tuple)
    end

    def initialize(basic_block, min=0, max=0, operands=[], name=nil)
      @min, @max = min, max

      super(basic_block, operands, name)
    end

    def pretty_parameters(p=LIR::PrettyPrinter.new)
      p.text    @min, '..', @max || '.'
      p.keyword 'of'
    end

    def has_side_effects?
      arguments_ty = arguments.type

      !(arguments_ty.is_a?(TupleType) &&
          arguments_ty.size &&
          arguments_ty.size.between?(@min, @max))
    end
  end
end