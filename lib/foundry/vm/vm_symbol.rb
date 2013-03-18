module Foundry
  class VMSymbol < VMImmediate
    attr_reader :value

    def initialize(value)
      @value = value.to_sym
    end

    alias to_sym value

    def class
      VI::Symbol
    end

    def hash
      @value.hash
    end

    def eql?(other)
      other.class == VI::Symbol &&
          other.value == @value
    end

    def inspect
      "{#{@value.inspect}}"
    end
  end
end
