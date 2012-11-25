module Foundry
  class VMObject < VMImmediate
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
      @ivar_table.fetch(ivar, VI::NIL)
    end

    def self.define_mapped_ivars(*set)
      set = set.map { |var| :"@#{var}" }

      # This basically adjusts cref to include receiver class.
      # Also, note that VMObject descends from BasicObject
      # which means that there is no host instance_variable_set.
      class_eval <<-EVAL, __FILE__ + '/define_mapped_ivars'
      def instance_variables
        #{set.inspect} + super
      end

      def instance_variable_get(ivar)
        #{set.each_with_index.map do |ivar, index| <<-IVAR
        #{'els' if index > 0}if ivar == #{ivar.inspect}
          #{ivar} || VI::NIL
          IVAR
        end.join}
        else
          super
        end
      end

      def instance_variable_set(ivar, value)
        #{set.each_with_index.map do |ivar, index| <<-IVAR
        #{'els' if index > 0}if ivar == #{ivar.inspect}
          #{ivar} = value
          IVAR
        end.join}
        else
          super
        end
      end
      EVAL
    end

    def inspect
      if @ivar_table.any?
        ivs = " "
        @ivar_table.each do |key, value|
          ivs << "#{key}=#{value}.inspect"
        end
      end

      "{#{@class.name}:#{__id__}#{ivs}}"
    end
  end
end
