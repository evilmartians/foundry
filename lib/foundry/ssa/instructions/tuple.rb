module Foundry::SSA
  class Tuple < Furnace::SSA::Instruction
    def use_count
      nil
    end

    def type
      VI::Foundry_Tuple
    end
  end
end