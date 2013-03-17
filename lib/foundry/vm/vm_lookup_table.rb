module Foundry
  class VMLookupTable < VMImmediate
    def initialize(storage={})
      @storage = storage
    end

    def self.vm_allocate
      new
    end

    def dup
      VMLookupTable.new(@storage.dup)
    end

    def class
      VI::LookupTable
    end

    def size
      @storage.size
    end

    def key?(key)
      @storage.has_key? key
    end

    def fetch(key, default=VI::NIL)
      @storage.fetch(key, default)
    end
    alias [] fetch

    def []=(key, value)
      @storage[key] = value
    end

    def each(&block)
      @storage.each &block
    end

    def keys
      VI.new_tuple(@storage.keys)
    end

    def to_h
      @storage
    end

    def inspect
      "{LUT #{@storage.inspect}}"
    end
  end
end
