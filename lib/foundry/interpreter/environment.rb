module Foundry::Interpreter
  class Environment
    def initialize(next_=nil)
      @next = next_
      @vars = {}
    end

    def each(&block)
      @vars.keys.each(&block)
      @next.each(&block) if @next
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
        raise "Undefined variable #{name.inspect}"
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
        raise "Undefined variable #{name.inspect}"
      end
    end

    def extend
      Environment.new(self)
    end
  end
end
