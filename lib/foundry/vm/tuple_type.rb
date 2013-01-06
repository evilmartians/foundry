module Foundry
  class TupleType
    attr_accessor :elements

    def initialize(elements)
      @elements = elements
    end

    def element_types
      @elements.map do |elem|
        elem.type if elem
      end
    end

    def reified?
      true
    end

    def ==(other)
      eql?(other)
    end

    def hash
      element_types.hash
    end

    def inspect_as_type
      types = element_types.map do |type|
        Furnace::SSA.inspect_type(type)
      end

      "^Tuple<#{types.join(', ')}>"
    end
  end
end