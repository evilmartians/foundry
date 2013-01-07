module Foundry
  class LIR::ConstRefInsn < Furnace::SSA::Instruction
    attr_accessor :constant

    syntax do |s|
      s.operand :cref, Monotype.of(VI::Tuple)
    end

    def initialize(basic_block, constant, operands=[], name=nil)
      super(basic_block, operands, name)
      @constant = constant
    end

    def pretty_parameters(p)
      p.text    @constant
      p.keyword 'in'
    end
  end
end