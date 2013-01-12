module Foundry
  class LiteralTupleType < TupleType
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
  end
end