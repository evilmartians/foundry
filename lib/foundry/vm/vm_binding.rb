module Foundry
  class VMBinding < VMObject
    def initialize(klass)
      super

      @vars = {}
      @next = nil
    end

    def vm_initialize(next_)
      @next = next_
    end

    def each(&block)
      @vars.keys.each(&block)
      @next.each(&block) if @next
    end

    def each_regular
      each do |var|
        yield var if var =~ /^[a-z]/
      end
    end

    def to_set
      set =  @vars.keys.to_set
      set += @next.to_set if @next
      set
    end

    def defined?(name)
      each do |var|
        return true if var == name
      end

      false
    end

    def apply(name)
      if @vars.has_key? name
        @vars[name]
      elsif @next
        @next.apply(name)
      else
        ::Kernel.raise "Undefined variable #{name.inspect}"
      end
    end

    def define(name, value)
      @vars[name] = value
    end

    def mutate(name, value)
      if @vars.has_key? name
        @vars[name] = value
      elsif @next
        @next.mutate(name, value)
      else
        ::Kernel.raise "Undefined variable #{name.inspect}"
      end
    end

    def chain
      VI::Binding.vm_new(self)
    end

    def inspect
      "{Binding}"
    end
  end
end