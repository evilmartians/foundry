module Foundry
  class LIR::CheckArityInsn < Furnace::SSA::Instruction
    attr_accessor :min, :max

    syntax do |s|
      s.operand :arguments
    end

    def initialize(min=0, max=0, operands=[], name=nil)
      @min, @max = min, max

      super(operands, name)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.text(@min).
        append('..').
        append(@max || '.').
        append(' ').
        keyword('of')
    end

    def has_side_effects?
      arguments.type.is_a?(Type::Tuple) &&
          !arguments.type.size.between?(@min, @max)
    end
  end
end
