module Foundry
  class LIR::TupleRefInsn < Furnace::SSA::Instruction
    attr_accessor :index

    syntax do |s|
      s.operand :tuple
    end

    def initialize(index, operands=[], name=nil)
      @index = index.to_i

      super(operands, name)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.text(@index).
        append(',')
    end

    def type
      if tuple.type.variable?
        Type.top
      else
        tuple.type.element_types[@index]
      end
    end
  end
end
