module Foundry
  class Type::Tuple < Type::Top
    attr_reader :element_types

    def initialize(element_types)
      @element_types = element_types.freeze
    end

    def replace_type_with(type, replacement)
      Type::Tuple.new(
          @element_types.map do |element_type|
            element_types.replace_type_with(type, replacement)
          end)
    end

    def size
      @element_types.size
    end

    def to_s
      "tuple<#{@element_types.join(', ')}>"
    end
  end
end
