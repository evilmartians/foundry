module Foundry
  class VMClass < VMModule
    attr_reader :superclass

    define_mapped_ivars :superclass

    def vm_initialize(superclass, vm_class=VMObject)
      super()

      @superclass = superclass
      @upperclass = superclass
      @vm_class   = vm_class
    end

    def vm_new(*args)
      if @vm_class.ancestors.include? VMObject
        instance = @vm_class.new(self)
        instance.vm_initialize(*args)
        instance
      else
        ::Kernel.send :raise, ::Exception, "cannot allocate VM object instance for VMImmediate"
      end
    end

    def inspect
      if @superclass
        sup = " < #{as_module_name @superclass.name, 'class'}}"
      end

      "{Class #{as_module_name @name, 'class'}#{sup}}"
    end
  end
end