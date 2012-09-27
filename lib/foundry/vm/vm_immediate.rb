module Foundry
  class VMImmediate < ::BasicObject
    def nil?
      false
    end

    def vm_nil?
      false
    end

    def vm_true?
      true
    end
  end
end