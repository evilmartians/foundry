module Foundry
  class LIR::TupleInsn < Furnace::SSA::Instruction
    def initialize(basic_block, operands=[], name=nil)
      @type = Type::Tuple.new(operands.map(&:type))

      super
    end

    def type
      @type
    end
  end
end
