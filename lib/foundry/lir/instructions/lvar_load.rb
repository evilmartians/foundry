module Foundry
  class LIR::LvarLoadInsn < Furnace::SSA::GenericInstruction
    attr_accessor :depth
    attr_accessor :variable

    syntax do |s|
      s.operand :binding, Monotype.of(VI::Binding)
    end

    def initialize(basic_block, type, depth, variable, operands=[], name=variable.to_s)
      super(basic_block, type, operands, name)
      @depth, @variable = depth.to_i, variable.to_sym
    end

    def pretty_parameters(p)
      p.text    @variable.inspect
      p.keyword 'at'
      p.text    @depth
      p.keyword 'in'
    end

    def type
      binding_ty = binding.type
      binding_ty.type_at @depth, @variable
    end
  end
end