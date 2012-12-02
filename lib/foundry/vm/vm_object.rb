module Foundry
  class VMObject < VMImmediate
    attr_reader :class

    def initialize(klass)
      @class           = klass
      @ivar_table      = VMLookupTable.new

      # Parts of initialization happen early enough so that
      # VI::NIL is not defined yet. All objects defined at this
      # stage are classes, and their singleton classes will be
      # overridden at initialization.
      @singleton_class = VI::NIL if defined?(VI::NIL)
    end

    def vm_initialize
    end

    def singleton_class_defined?
      !@singleton_class.nil?
    end

    def singleton_class
      if @singleton_class.nil?
        @singleton_class = VI::Foundry_SingletonClass.vm_new(self.class, self)
      end

      @singleton_class
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
        VI.new_tuple(#{set.inspect} + super.to_a)
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
      if @ivar_table.size > 0
        ivs = " "
        @ivar_table.each do |key, value|
          ivs << "#{key}=#{value.inspect}"
        end
      end

      "{#{@class.name}:#{__id__}#{ivs}}"
    end
  end
end
