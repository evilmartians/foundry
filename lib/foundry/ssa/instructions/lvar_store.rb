module Foundry
  class SSA::LvarStoreInsn < Furnace::SSA::Instruction
    attr_accessor :depth
    attr_accessor :variable

    syntax do |s|
      s.operand :binding, VI::Binding
      s.operand :value
    end

    def initialize(basic_block, depth, variable, operands=[], name=nil)
      super(basic_block, operands, name)
      @depth, @variable = depth, variable
    end

    def pretty_parameters(p)
      p.text    @variable.inspect
      p.keyword 'at'
      p.text    @depth
      p.keyword 'in'
    end

    def use_count
      4
    end
  end
end