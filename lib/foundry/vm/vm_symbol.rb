module Foundry
  class VMSymbol < VMObject
    attr_reader :value

    def vm_initialize(value)
      @value = value.to_sym
    end

    alias to_sym value

    def inspect
      "{#{@value.inspect}}"
    end
  end
end