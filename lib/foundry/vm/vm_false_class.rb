module Foundry
  class VMFalseClass < VMImmediate
    def class
      VI::FalseClass
    end

    def inspect
      "{false}"
    end
  end
end