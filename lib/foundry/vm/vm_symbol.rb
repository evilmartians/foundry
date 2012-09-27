module Foundry
  class VMSymbol < VMImmediate
    attr_reader :value

    def initialize(klass, value)
      @value = value.to_sym
    end

    def class
      VM::Symbol
    end

    def inspect
      "{:#{@value}}"
    end
  end
end