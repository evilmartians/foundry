module Foundry
  class VMInteger < VMObject
    attr_reader :value

    def vm_initialize(value)
      @value = value.to_int
    end

    alias to_i value

    def inspect
      "{#{@value}}"
    end
  end
end