module Foundry
  class Type::Tuple < Type::Top
    attr_reader :element_types

    def initialize(element_types)
      @element_types = element_types.freeze
    end

    def replace_type_with(type, replacement)
      Type::Tuple.new(
          @element_types.map do |element_type|
            element_type.replace_type_with(type, replacement)
          end)
    end

    def size
      @element_types.size
    end

    def to_klass
      VI::Tuple
    end

    def specialize(other)
      if other.is_a?(Type::Tuple) && size == other.size
        element_types.
            zip(other.element_types).
            map do |type, other_type|
              type.specialize(other_type)
            end.
            reduce(:merge)
      else
        super
      end
    end

    def to_s
      Furnace::AwesomePrinter.new(false) do |p|
        awesome_print(p)
      end
    end

    def awesome_print(p=Furnace::AwesomePrinter.new)
      p.type 'tuple'

      p.collection('<', ', ', '>', @element_types)
    end
  end
end
