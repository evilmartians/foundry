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
      Furnace::AwesomePrinter.new(false) do |p|
        awesome_print(p)
      end
    end

    def awesome_print(p=Furnace::AwesomePrinter.new)
      p.type 'lut'

      p << '<'
      @element_types.each do |key, value_type|
        p.text key
        p <<   ':'
        value_type.awesome_print p
      end
      p.text '>'
    end
  end
end
