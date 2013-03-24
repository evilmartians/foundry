module Foundry
  class LIR::TupleBiggerInsn < Furnace::SSA::Instruction
    attr_accessor :min_size

    syntax do |s|
      s.operand :tuple
    end

    def initialize(min_size, operands=[], name=nil)
      @min_size = min_size.to_i

      super(operands, name)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.text @min_size, ','
    end

    def type
      Type.klass(VI::Object)
    end
  end
end
