module Foundry
  class LIR::LvarStoreInsn < Furnace::SSA::Instruction
    attr_accessor :depth
    attr_accessor :variable

    syntax do |s|
      s.operand :binding
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

    def valid?(ignore_nil_types=true)
      super && begin
        binding_ty  = binding.type

        variable_ty = binding_ty.type_at(@depth, @variable)
        value_ty    = value.type

        if variable_ty.nil? || value_ty.nil?
          ignore_nil_types
        else
          variable_ty == value_ty
        end
      end
    end
  end
end