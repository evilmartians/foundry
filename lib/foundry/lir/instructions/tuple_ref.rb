module Foundry
  class LIR::TupleRefInsn < Furnace::SSA::Instruction
    attr_accessor :index

    syntax do |s|
      s.operand :tuple
    end

    def initialize(basic_block, index, operands=[], name=nil)
      super(basic_block, operands, name)
      @index = index
    end

    def pretty_parameters(p)
      p.text @index, ','
    end

    def type
      tuple_ty = tuple.type

      if tuple_ty.reified?
        tuple_ty.element_types[@index]
      end
    end
  end
end