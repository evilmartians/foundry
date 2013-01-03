module Foundry
  class SSA::BindingInsn < Furnace::SSA::Instruction
    attr_accessor :variables

    syntax do |s|
      s.operand :binding, VI::Binding
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
      VI::Binding
    end
  end
end