module Foundry
  class VMBasicObject < VMImmediate
    attr_reader :class

    def initialize(klass)
      @class      = klass
      @ivar_table = {}
    end

    def instance_variables
      @ivar_table.keys
    end

    def instance_variable_set(ivar, value)
      @ivar_table[ivar] = value
    end

    def instance_variable_get(ivar)
      @ivar_table[ivar]
    end

    def inspect
      if @ivar_table.any?
        ivs = " "
        @ivar_table.each do |key, value|
          ivs << "@#{key}=#{value}.inspect"
        end
      end

      "{#{@class.name}:#{__id__}#{ivs}}"
    end
  end
end
