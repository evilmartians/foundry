module Foundry
  class ClosureType < LIR::Type
    attr_accessor :arguments
    attr_accessor :block
    attr_accessor :return_type

    def initialize(arguments=nil, block=nil, return_type=nil)
      @arguments, @block = arguments, block
      @return_type = return_type
    end

    def monotype?
      arguments && arguments.monotype? &&
          block && block.monotype? &&
          return_type && return_type.monotype?
    end

    def subtype_of?(other)
      super ||
          other.instance_of?(Monotype) &&
          other.klass == VI::Proc
    end

    def inspect
      args_s  = arguments ? arguments.inspect : '?'
      block_s = block ? block.inspect : '?'
      ret_s   = return_type ? return_type.inspect : '?'

      "closure<#{args_s}, &#{block_s} -> #{ret_s}>"
    end
  end
end