module Foundry
  class SSA::TupleBiggerInsn < Furnace::SSA::Instruction
    attr_accessor :index

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

    def use_count
      1
    end

    def type
      VI::Object
    end
  end
end