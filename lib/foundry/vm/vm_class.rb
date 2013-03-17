module Foundry
  class VMClass < VMModule
    attr_reader :parameters, :specializations

    define_mapped_ivars :parameters

    def vm_initialize(superclass, vm_class=VMObject)
      super()

      @superclass = superclass
      @vm_class   = vm_class

      if @superclass.nil?
        @parameters      = VMTuple.new([ :by_value ])
        @specializations = VMLookupTable.new
      else
        @parameters      = @superclass.parameters
        @specializations = @superclass.specializations.dup
      end
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
      specializations_s = @specializations.to_h.
          map do |name, value|
            "#{name}=#{value.inspect}"
          end.
          join(', ')

      "{class #{as_module_name @name, 'class'}#{specializations_s}}"
    end
  end
end
