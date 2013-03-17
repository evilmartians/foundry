module Foundry
  class LIR::ConstFetchInsn < Furnace::SSA::GenericInstruction
    attr_accessor :constant

    syntax do |s|
      s.operand :scope
    end

    def initialize(basic_block, constant, type, operands=[], name=nil)
      @constant = constant

      super(basic_block, type, operands, name)
    end

    def pretty_parameters(p=LIR::PrettyPrinter.new)
      p.text    @constant
      p.keyword 'from'
    end
  end
end
