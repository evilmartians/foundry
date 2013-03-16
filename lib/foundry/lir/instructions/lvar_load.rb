module Foundry
  class LIR::LvarLoadInsn < Furnace::SSA::GenericInstruction
    attr_accessor :depth
    attr_accessor :variable

    syntax do |s|
      s.operand :binding#, Type.klass(VI::Binding)
    end

    def initialize(basic_block, type, depth, variable, operands=[], name=variable.to_s)
      @depth, @variable = depth.to_i, variable.to_sym

      super(basic_block, type, operands, name)
    end

    def pretty_parameters(p=LIR::PrettyPrinter.new)
      p.text    @variable.inspect
      p.keyword 'at'
      p.text    @depth
      p.keyword 'in'
    end
  end
end
