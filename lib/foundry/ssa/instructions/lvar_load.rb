module Foundry::SSA
  class LvarLoad < Furnace::SSA::GenericInstruction
    attr_accessor :depth
    attr_accessor :variable

    def initialize(basic_block, type, depth, variable, operands=[], name=nil)
      super(basic_block, type, operands, name)
      @depth, @variable = depth, variable
    end

    def pretty_parameters(p)
      p.text    @variable.inspect
      p.keyword 'at'
      p.text    @depth
      p.keyword 'in'
    end

    def use_count
      3
    end
  end
end