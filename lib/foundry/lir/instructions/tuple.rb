module Foundry
  class LIR::TupleInsn < Furnace::SSA::Instruction
    def type
      VI::Foundry_Tuple
    end
  end
end