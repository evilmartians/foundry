module Foundry
  class LIR::ReifyInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass
      s.operand :specializations
    end

    def type
      Type.top
    end
  end
end
