module Foundry
  class Type::Binding < Type::Top
    attr_reader :variables, :next

    def self.normalize(elements, next_)
      # Accept both a Hash or an Array of names.
      [ Hash[elements.map { |elem| Array(elem) }],
        next_ ]
    end

    def initialize(elements, next_)
      @variables = elements.freeze
      @next      = next_
    end

    def to_static_env
      set = [ @variables.keys.to_set ]

      if @next.nil?
        set
      else
        set + @next.to_static_env
      end
    end

    def type_at(depth, name)
      if depth > 0
        @next.type_at(depth - 1, name)
      elsif @variables.key? name
        @variables[name]
      else
        raise ArgumentError, "binding for #{name} not found"
      end
    end

    def variable_types
      @variables.values
    end

    def index_of(name)
      @variables.keys.index(name)
    end

    def replace_type_with(type, replacement)
      unless @next.nil?
        next_type = @next.replace_type_with(type, replacement)
      else
        next_type = @next
      end

      Type::Binding.new(
          Hash[@variables.map do |name, var_type|
                 [name, var_type.replace_type_with(type, replacement)]
               end],
          next_type)
    end

    def to_s
      LIR::PrettyPrinter.new(false) do |p|
        pretty_print(p)
      end
    end

    def pretty_print(p=LIR::PrettyPrinter.new)
      p.type 'binding'

      p << '<'

      p.objects(@variables) do |name, type|
        p.text "#{name}:"
        type.pretty_print(p)
      end
      p << ', ... ' if @next

      p << '>'
    end
  end
end
