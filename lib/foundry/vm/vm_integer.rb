module Foundry
  class VMInteger < VMNumeric
    attr_reader :value

    def initialize(klass, value)
      super(klass)

      @value = value.to_int
    end

    alias to_int value

    def inspect
      "{#{@value}}"
    end
  end
end