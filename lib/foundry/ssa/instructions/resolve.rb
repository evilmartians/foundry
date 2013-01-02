module Foundry::SSA
  class Resolve < Furnace::SSA::Instruction
    def use_count
      2
    end

    def type
      VI::Proc
    end
  end
end