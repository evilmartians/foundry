module Foundry
  class TupleType
    def initialize(elements)
      @elements = elements
    end

    def element_types
      @elements.map do |elem|
        elem.type if elem
      end
    end

    def inspect_as_type
      types = element_types.map do |type|
        Furnace::SSA.inspect_type(type)
      end

      "^Tuple<#{types.join(', ')}>"
    end
  end
end