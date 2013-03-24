module Foundry
  class LIR::LvarLoadInsn < Furnace::SSA::GenericInstruction
    attr_accessor :depth
    attr_accessor :variable

    syntax do |s|
      s.operand :binding
    end

    def initialize(type, depth, variable, operands=[], name=variable.to_s)
      @depth, @variable = depth.to_i, variable.to_sym

      super(type, operands, name)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.text    @variable.inspect
      p.keyword 'at'
      p.text    @depth
      p.keyword 'in'
    end
  end
end
