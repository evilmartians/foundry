module Foundry
  class VMSymbol < VMImmediate
    attr_reader :value

    def initialize(value)
      @value = value.to_sym
    end

    def class
      VM::Symbol
    end

    def inspect
      "{#{@value.inspect}}"
    end

    def _equal?(interp, scope)
      other = scope.arguments.first
      if other.is_a? VI::Symbol
        (@value == other.value) ? VI::TRUE : VI::FALSE
      else
        VI::FALSE
      end
    end
  end
end