module Foundry
  class LIR::ClosureInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :binding, Type.klass(VI::Binding)
      s.operand :callee,  LIR::Function
    end

    def initialize(basic_block, operands=[], name=nil)
      super
    end

    def type
      nil # FIXME
    end
  end
end
