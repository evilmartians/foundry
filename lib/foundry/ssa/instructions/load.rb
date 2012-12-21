module Foundry::SSA
  class Load < Furnace::SSA::Instruction
    def use_count
      2
    end

    def def_count
      1
    end
  end
end