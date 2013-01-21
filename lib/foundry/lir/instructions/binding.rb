module Foundry
  class LIR::BindingInsn < Furnace::SSA::Instruction
    attr_accessor :variables

    syntax do |s|
      s.operand :next
    end

    def initialize(basic_block, variables=[], operands=[], name=nil)
      @variables = variables
      @type      = nil

      super(basic_block, operands, name)
    end

    def initialize_copy(original)
      super

      @type = nil
    end

    def pretty_parameters(p=LIR::PrettyPrinter.new)
      p.text    @variables.map(&:inspect).join(", ")
      p.keyword 'chain'
    end

    def type
      @type ||= BindingType.new(@variables, self.next.type)
    end

    def reset_type!
      @type = nil
    end
  end
end