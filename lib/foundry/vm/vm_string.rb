module Foundry
  class VMString < VMObject
    attr_reader :value

    def initialize(klass, value)
      super(klass)

      @value = value.to_str
    end

    def inspect
      "{#{@value.inspect}}"
    end
  end
end