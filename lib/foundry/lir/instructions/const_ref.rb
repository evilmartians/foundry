module Foundry
  class LIR::ConstRefInsn < Furnace::SSA::GenericInstruction
    attr_accessor :constant

    syntax do |s|
      s.operand :cref
    end

    def initialize(constant, operands=[], name=nil)
      @constant = constant

      super(operands, name)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.text    @constant
      p.keyword 'in'
    end
  end
end
