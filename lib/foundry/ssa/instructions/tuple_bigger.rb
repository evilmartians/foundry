module Foundry::SSA
  class TupleBigger < Furnace::SSA::Instruction
    attr_accessor :index

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