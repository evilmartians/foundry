module Foundry
  class SSA::TupleInsn < Furnace::SSA::Instruction
    def type
      VI::Foundry_Tuple
    end
  end
end