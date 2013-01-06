module Foundry
  class BindingType
    attr_reader :variables, :next

    def initialize(elements, next_)
      # Accept both a Hash or an Array of names.
      @variables = Hash[elements.map { |elem| Array(elem) }]
      @next      = next_
    end

    def to_static_env
      set = [ @variables.keys.to_set ]
      if @next
        set + @next.to_static_env
      else
        set
      end
    end

    def type_at(depth, name)
      if depth > 0
        @next.type_at(depth - 1, name)
      elsif @variables.key? name
        @variables[name]
      end
    end

    def reified?
      true
    end

    def ==(other)
      eql?(other)
    end

    def hash
      [@variables, @next].hash
    end

    def inspect_as_type
      types = @variables.map do |name, type|
        "#{name}: #{Furnace::SSA.inspect_type(type)}"
      end

      continuation = ", ..." if @next

      "^Binding<#{types.join(', ')}#{continuation}>"
    end
  end
end