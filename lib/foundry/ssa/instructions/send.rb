module Foundry::SSA
  class Send < Furnace::SSA::Instruction
    def use_count
      4
    end

    def def_count
      1
    end
  end
end