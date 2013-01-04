module Foundry
  class LIR::Builder < Furnace::SSA::Builder
    def self.scope
      LIR
    end
  end
end