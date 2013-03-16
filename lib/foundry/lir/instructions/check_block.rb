module Foundry
  class LIR::CheckBlockInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :block, Type.klass(VI::Proc)
    end

    def has_side_effects?
      true
    end
  end
end
