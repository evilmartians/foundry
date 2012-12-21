module Foundry::SSA
  class Tuple < Furnace::SSA::Instruction
    def use_count
      nil
    end

    def def_count
      1
    end
  end
end