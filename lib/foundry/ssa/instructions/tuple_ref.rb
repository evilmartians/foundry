module Foundry
  class SSA::TupleRefInsn < Furnace::SSA::Instruction
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

    def use_count
      1
    end

    def type
      nil
    end
  end
end