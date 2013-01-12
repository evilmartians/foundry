module Foundry
  class TupleType < LIR::GenericType
    def element_types
      raise NotImplementedError, "implement #{self.class}#element_types"
    end

    def parameters
      element_types
    end

    def size
      element_types && element_types.length
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
      if element_types
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