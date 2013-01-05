module Foundry
  class LIR::LvarStoreInsn < Furnace::SSA::Instruction
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

    def has_side_effects?
      true
    end
  end
end