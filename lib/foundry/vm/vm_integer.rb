module Foundry
  class VMInteger < VMObject
    attr_reader :value

    def vm_initialize(value)
      @value = value.to_int
    end

    alias to_int value
    alias to_i   value

    def hash
      @value.hash
    end

    def eql?(other)
      other.respond_to?(:to_int) &&
          @value == other.to_int
    end

    def inspect
      "{#{@value}}"
    end
  end
end
