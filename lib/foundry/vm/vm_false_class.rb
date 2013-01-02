module Foundry
  class VMFalseClass < VMImmediate
    def class
      VI::FalseClass
    end

    def self.inspect_as_type
      "^FalseClass"
    end

    def inspect
      "{false}"
    end
  end
end