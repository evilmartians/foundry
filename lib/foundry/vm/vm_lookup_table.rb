module Foundry
  class VMLookupTable < VMImmediate
    def initialize
      @storage = {}
    end

    def self.vm_allocate
      new
    end

    def class
      VI::Foundry_LookupTable
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

    def inspect
      "{LUT #{@storage.inspect}}"
    end
  end
end