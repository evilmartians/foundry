module Foundry
  class LIR::TraceInsn < Furnace::SSA::Instruction
    def has_side_effects?
      true
    end

    def type
      Type.of(VI::NIL)
    end
  end
end
