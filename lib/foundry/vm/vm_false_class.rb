module Foundry
  class VMFalseClass < VMImmediate
    def class
      VI::FalseClass
    end

    def vm_true?
      false
    end

    def inspect
      "{false}"
    end
  end
end