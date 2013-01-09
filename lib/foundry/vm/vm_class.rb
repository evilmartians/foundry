module Foundry
  class VMClass < VMModule
    def vm_initialize(superclass, vm_class=VMObject)
      super()

      @superclass = superclass
      @vm_class   = vm_class
    end

    def vm_allocate
      if @vm_class.nil?
        ::Kernel.send :raise, ::Exception, "cannot allocate instances of class #{self.inspect}"
      elsif @vm_class.ancestors.include? VMObject
        @vm_class.new(self)
      else
        @vm_class.new
      end
    end

    def vm_new(*args)
      if @vm_class.ancestors.include? VMObject
        instance = vm_allocate
        instance.vm_initialize(*args)
        instance
      else
        @vm_class.new(*args)
      end
    end

    def superclass
      klass = @superclass

      while klass.is_a? VI::IncludedModule
        klass = klass.direct_superclass
      end

      klass
    end

    def singleton_class
      if @singleton_class.nil?
        if @superclass.nil?
          # I am BasicObject.
          @singleton_class = VI::SingletonClass.vm_new(VI::Class, self)
        else
          @singleton_class = VI::SingletonClass.vm_new(superclass.singleton_class, self)
        end
      end

      @singleton_class
    end

    def inspect
      unless @superclass.nil?
        sup = " < #{as_module_name @superclass.name, 'class'}"
      end

      "{class #{as_module_name @name, 'class'}#{sup}}"
    end
  end
end