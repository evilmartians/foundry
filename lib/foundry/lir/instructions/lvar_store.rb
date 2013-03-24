module Foundry
  class LIR::LvarStoreInsn < Furnace::SSA::Instruction
    attr_accessor :depth
    attr_accessor :variable

    syntax do |s|
      s.operand :binding
      s.operand :value
    end

    def initialize(depth, variable, operands=[], name=nil)
      @depth, @variable = depth.to_i, variable.to_sym

      super(operands, name)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
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
