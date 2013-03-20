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
      other.respond_to?(:to_sym) &&
          @value == other.to_sym
    end

    def inspect
      "{#{@value.inspect}}"
    end
  end
end
