module Foundry
  class VMClass < VMModule
    def vm_initialize(superclass, vm_class=VMObject)
      # Parts of initialization happen early enough that VI::NIL
      # is not defined. The name is overridden later anyway, and
      # host `nil` responds to #nil? just as well.
      @name           = VI::NIL if defined?(VI::NIL)
      @superclass     = superclass

      @constant_table = {}
      @method_table   = {}

      @vm_class       = vm_class
    end

    def vm_allocate
      if @vm_class.nil?
        ::Kernel.send :raise, ::Exception, "cannot allocate instances of class #{self.inspect}"
      else
        @vm_class.new(self)
      end
    end

    def vm_new(*args)
      if @vm_class.ancestors.include? VMObject
        instance = vm_allocate
        instance.vm_initialize(*args)
        instance
      else
        ::Kernel.send :raise, ::Exception, "cannot allocate VM object instance for VMImmediate"
      end
    end

    def singleton_class
      if @singleton_class.nil?
        if @superclass.nil?
          # I am BasicObject.
          @singleton_class = VI::Foundry_SingletonClass.vm_new(VI::Class, self)
        else
          @singleton_class = VI::Foundry_SingletonClass.vm_new(@superclass.singleton_class, self)
        end
      end

      @singleton_class
    end

    def inspect
      unless @superclass.nil?
        sup = " < #{as_module_name @superclass.name, 'class'}"
      end

      "{Class #{as_module_name @name, 'class'}#{sup}}"
    end
  end
end