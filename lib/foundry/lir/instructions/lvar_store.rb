module Foundry
  class LIR::LvarStoreInsn < Furnace::SSA::Instruction
    attr_accessor :depth
    attr_accessor :variable

    syntax do |s|
      s.operand :binding#, Type.klass(VI::Binding)
      s.operand :value
    end

    def initialize(basic_block, depth, variable, operands=[], name=nil)
      @depth, @variable = depth.to_i, variable.to_sym

      super(basic_block, operands, name)
    end

    def pretty_parameters(p=LIR::PrettyPrinter.new)
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
