module Foundry
  class Type::Closure < Type::Top
    attr_reader :arguments_type, :block_type, :return_type

    def self.normalize(arguments_type, block_type, return_type)
      [ arguments_type.to_type,
        block_type.to_type,
        return_type.to_type ]
    end

    def initialize(arguments_type, block_type, return_type)
      @arguments_type = arguments_type
      @block_type     = block_type
      @return_type    = return_type
    end

    def replace_type_with(type, replacement)
      Type::Closure.new(
          @arguments_type.replace_type_with(type, replacement),
          @block_type.replace_type_with(type, replacement),
          @return_type.replace_type_with(type, replacement))
    end

    def to_klass
      VI::Proc
    end

    def to_s
      "closure<#{arguments_type}, &#{block_type} -> #{return_type}>"
    end
  end
end
