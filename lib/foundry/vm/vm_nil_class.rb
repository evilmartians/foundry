module Foundry
  class VMNilClass < VMImmediate
    def class
      VI::NilClass
    end

    def self.inspect_as_type
      "^NilClass"
    end

    def inspect
      "{nil}"
    end
  end
end