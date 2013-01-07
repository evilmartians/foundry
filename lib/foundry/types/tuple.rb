module Foundry
  class TupleType < LIR::GenericType
    attr_accessor :elements

    def initialize(elements=nil)
      @elements = elements
    end

    def element_types
      if @elements
        @elements.map do |elem|
          Foundry.typeof(elem) if elem
        end
      end
    end

    alias parameters element_types

    def size
      @elements && @elements.length
    end

    def monotype?
      element_types && element_types.all?
    end

    def subtype_of?(other)
      super ||
          other.instance_of?(Monotype) &&
          other.klass == VI::Tuple
    end

    def inspect
      if @elements
        types = element_types.map do |type|
          Furnace::SSA.inspect_type(type)
        end.join(', ')
      else
        types = '?'
      end

      "tuple<#{types}>"
    end
  end
end