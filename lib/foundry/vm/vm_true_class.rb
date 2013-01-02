module Foundry
  class VMTrueClass < VMImmediate
    def class
      VI::TrueClass
    end

    def self.inspect_as_type
      "^TrueClass"
    end

    def inspect
      "{true}"
    end
  end
end