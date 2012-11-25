module Foundry
  class VMTuple < VMObject
    def vm_initialize(value)
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