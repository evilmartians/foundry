module Foundry
  class VMNilClass < VMImmediate
    def class
      VI::NilClass
    end

    def vm_nil?
      true
    end

    def vm_true?
      false
    end

    def inspect
      "{nil}"
    end
  end
end