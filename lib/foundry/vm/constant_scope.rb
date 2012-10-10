module Foundry
  class ConstantScope < VMImmediate
    attr_reader :nesting

    def initialize(nesting)
      @nesting = nesting.to_a.dup.freeze
    end

    def find_const(name)
      @nesting.each do |klass|
        value = klass.const_get(name)
        return value if value
      end
    end

    def nest(klass)
      ConstantScope.new([ klass ] + @nesting)
    end

    def inspect
      "{ConstantScope #{@nesting}}"
    end
  end
end