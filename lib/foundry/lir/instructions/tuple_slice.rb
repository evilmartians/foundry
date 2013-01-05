module Foundry
  class LIR::TupleSliceInsn < Furnace::SSA::Instruction
    attr_accessor :from, :to

    syntax do |s|
      s.operand :tuple
    end

    def initialize(basic_block, from, to, operands=[], name=nil)
      super(basic_block, operands, name)
      @from, @to = from, to
    end

    def pretty_parameters(p)
      p.text @from, '..', @to, ','
    end

    def type
      tuple_ty = tuple.type

      if tuple_ty.reified?
        tuple_ty.element_types[@from..@to]
      end
    end
  end
end