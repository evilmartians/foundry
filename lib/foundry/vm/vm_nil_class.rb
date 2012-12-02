module Foundry
  class VMNilClass < VMImmediate
    def class
      VI::NilClass
    end

    def inspect
      "{nil}"
    end
  end
end