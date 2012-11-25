module Foundry
  class VMClass < VMModule
    attr_reader :superclass

    def vm_initialize(superclass, name, vm_class)
      super(name)

      @vm_class   = vm_class
      @superclass = superclass
      @upperclass = superclass
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
        "{Class #{@name} < #{@superclass.name}}"
      else
        "{Class #{@name}}"
      end
    end
  end
end