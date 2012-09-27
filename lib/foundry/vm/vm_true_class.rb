module Foundry
  class VMTrueClass < VMImmediate
    def class
      VI::TrueClass
    end

    def inspect
      "{true}"
    end
  end
end