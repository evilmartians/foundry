module Foundry
  class VMTuple < VMImmediate
    def initialize(value)
      @storage = value.to_ary.freeze
    end

    def class
      VI::Tuple
    end

    def size
      @storage.size
    end

    def [](index)
      @storage.fetch(index.to_int, VI::NIL)
    end

    def each(&block)
      @storage.each &block
    end

    def to_a
      @storage
    end
    alias to_ary to_a

    def +(other)
      VI.new_tuple(@storage + other.to_a)
    end

    def uniq
      VI.new_tuple(@storage.uniq)
    end

    def inspect
      "{Tuple #{@storage.inspect}}"
    end
  end
end
