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
  end
end
