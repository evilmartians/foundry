module Foundry
  class LIR::ConstFetchInsn < Furnace::SSA::Instruction
    attr_accessor :constant

    syntax do |s|
      s.operand :scope
    end

    def initialize(basic_block, constant, operands=[], name=nil)
      super(basic_block, operands, name)
      @constant = constant
    end

    def pretty_parameters(p)
      p.text    @constant
      p.keyword 'from'
    end
  end
end