module Foundry
  class LIR::ConstFetchInsn < Furnace::SSA::GenericInstruction
    attr_accessor :constant

    syntax do |s|
      s.operand :scope
    end

    def initialize(constant, type, operands=[], name=nil)
      @constant = constant

      super(type, operands, name)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.text    @constant
      p.keyword 'from'
    end
  end
end
