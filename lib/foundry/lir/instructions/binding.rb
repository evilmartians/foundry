module Foundry
  class LIR::BindingInsn < Furnace::SSA::Instruction
    attr_accessor :variables

    syntax do |s|
      s.operand :binding
    end

    def initialize(basic_block, variables=[], operands=[], name=nil)
      super(basic_block, operands, name)
      @variables = variables
    end

    def pretty_parameters(p)
      p.text    @variables.map(&:inspect).join(", ")
      p.keyword 'chain'
    end

    def type
      BindingType.new(@variables, binding.type)
    end
  end
end