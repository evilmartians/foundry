module Foundry
  class Monotype < LIR::GenericType
    @identity_map = Hash.new do |map, klass|
      map[klass] = Monotype.new(klass)
    end

    def self.of(klass)
      if defined?(LIR::LvarLoadInsn) &&
           klass == LIR::LvarLoadInsn
        raise
      end
      @identity_map[klass]
    end

    attr_reader :klass

    def initialize(klass)
      @klass = klass
    end

    def parameters
      [@klass]
    end

    def inspect
      "^#{@klass.name}"
    end
  end
end