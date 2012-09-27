module Foundry
  class VMNilClass < VMImmediate
    def class
      VI::NilClass
    end

    def nil?
      true
    end

    def inspect
      "{nil}"
    end
  end
end