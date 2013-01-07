module Foundry
  class LIR::ClosureInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :binding, Monotype.of(VI::Binding)
      s.operand :callee,  LIR::Function
    end

    def initialize(basic_block, operands=[], name=nil)
      @type = nil

      super
    end

    def type
      @type = ClosureType.new
    end

    def reset_type!
      @type = nil
    end
  end
end