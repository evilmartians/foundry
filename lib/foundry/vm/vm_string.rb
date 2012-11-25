module Foundry
  class VMString < VMObject
    attr_reader :value

    def vm_initialize(value)
      @value = value.to_str
    end

    alias to_s value

    def inspect
      "{#{@value.inspect}}"
    end
  end
end