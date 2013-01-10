module Foundry
  class ClosureType < LIR::Type
    attr_accessor :arguments_type
    attr_accessor :block_type
    attr_accessor :return_type

    def initialize(arguments_type=nil, block_type=nil, return_type=nil)
      @arguments_type = arguments_type
      @block_type     = block_type
      @return_type    = return_type
    end

    def monotype?
      arguments_type  && arguments_type.monotype? &&
          block_type  && block_type.monotype?     &&
          return_type && return_type.monotype?
    end

    def subtype_of?(other)
      super ||
          other.instance_of?(Monotype) &&
          other.klass == VI::Proc
    end

    def inspect
      args_s  = arguments_type ? arguments_type.inspect : '?'
      block_s = block_type     ? block_type.inspect     : '?'
      ret_s   = return_type    ? return_type.inspect    : '?'

      "closure<#{args_s}, &#{block_s} -> #{ret_s}>"
    end
  end
end