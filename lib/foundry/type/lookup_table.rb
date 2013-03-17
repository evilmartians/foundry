module Foundry
  class Type::LookupTable < Type::Top
    def initialize(element_types)
      @element_types = element_types.freeze
    end

    def replace_type_with(type, replacement)
      Type::LookupTable.new(Hash[
          @element_types.map do |key, value_type|
            [ key, value_type.
                    replace_type_with(type, replacement) ]
          end])
    end

    def size
      @element_types.size
    end

    def to_klass
      VI::LookupTable
    end

    def to_s
      LIR::PrettyPrinter.new(false) do |p|
        pretty_print(p)
      end
    end

    def pretty_print(p=LIR::PrettyPrinter.new)
      p.type 'lut'

      p << '<'
      @element_types.each do |key, value_type|
        p.text key
        p <<   ':'
        value_type.pretty_print p
      end
      p.text '>'
    end
  end
end
