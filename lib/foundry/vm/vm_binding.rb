module Foundry
  class VMBinding < VMImmediate
    def initialize(next_=nil)
      @vars = {}
      @next = next_
    end

    def class
      VI::Binding
    end

    def each(&block)
      @vars.keys.each(&block)
      @next.each(&block) unless @next.nil?
    end

    def each_regular
      each do |var|
        yield var if var =~ /^[a-z]/
      end
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
      elsif !@next.nil?
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

    def __type__
      if @next.nil?
        next_type = @next
      else
        next_type = @next.__type__
      end

      @type ||= Type::Binding.new(
                  @vars.map do |name, value|
                    [ name, Type.of(value) ]
                  end,
                  next_type)
    end

    def inspect
      "{Binding:#{__id__}}"
    end
  end
end
