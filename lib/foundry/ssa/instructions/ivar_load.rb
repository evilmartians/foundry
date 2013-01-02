module Foundry::SSA
  class IvarLoad < Furnace::SSA::GenericInstruction
    def use_count
      2
    end
  end
end