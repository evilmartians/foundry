module Foundry
  class VMSymbol < VMImmediate
    attr_reader :value

    def initialize(value)
      @value = value.to_sym
    end

    def class
      VI::Symbol
    end

    def inspect
      "{#{@value.inspect}}"
    end
  end
end