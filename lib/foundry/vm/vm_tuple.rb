module Foundry
  class VMTuple < VMObject
    def initialize(klass, value)
      super(klass)

      @storage = value.to_ary.freeze
    end

    def [](index)
      @storage[index.to_int] || VI::NIL
    end

    def length
      @storage.length
    end

    def each(&block)
      @storage.each &block
    end

    def to_a
      @storage
    end
    alias to_ary to_a

    def inspect
      "{Tuple #{@storage.inspect}}"
    end
  end
end