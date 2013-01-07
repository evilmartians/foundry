module Foundry
  class LIR::ConstRefInsn < Furnace::SSA::Instruction
    attr_accessor :constant

    syntax do |s|
      s.operand :cref, Monotype.of(VI::Tuple)
    end

    def initialize(basic_block, constant, operands=[], name=nil)
      @constant = constant

      super(basic_block, operands, name)
    end

    def pretty_parameters(p)
      p.text    @constant
      p.keyword 'in'
    end
  end
end