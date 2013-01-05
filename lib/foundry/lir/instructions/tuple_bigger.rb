module Foundry
  class LIR::TupleBiggerInsn < Furnace::SSA::Instruction
    attr_accessor :size

    syntax do |s|
      s.operand :tuple
    end

    def initialize(basic_block, size, operands=[], name=nil)
      super(basic_block, operands, name)
      @size = size
    end

    def pretty_parameters(p)
      p.text @size, ','
    end

    def type
      tuple_ty = tuple.type

      if tuple_ty.reified?
        if tuple_ty.elements.count > @size
          VI::TrueClass
        else
          VI::FalseClass
        end
      end
    end
  end
end