module Foundry
  class VMClass < VMModule
    attr_reader :vm_class

    attr_reader :parameters, :specializations

    define_mapped_ivars :parameters

    def vm_initialize(superclass, vm_class=VMObject)
      super()

      @superclass = superclass
      @vm_class   = vm_class

      if @superclass.nil?
        @parameters      = VMTuple.new([ VMSymbol.new(:by_value) ])
        @specializations = VMLookupTable.new
      else
        @parameters      = @superclass.parameters
        @specializations = @superclass.specializations.dup
      end
    end

    def initialize_copy(original)
      @class             = original.class
      @name, @superclass = original.name, original.superclass
      @vm_class          = original.vm_class
      @method_table      = original.method_table
      @constant_table    = original.constant_table

      @parameters        = original.parameters
      @specializations   = original.specializations.dup
    end

    def dup
      instance = VMClass.allocate
      instance.__send__ :initialize_copy, self
      instance
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

    def reify(specializations)
      reified_klass = dup

      specializations.each do |key, value|
        reified_klass.specializations[key] = value
      end

      reified_klass
    end

    def inspect
      if @specializations.size > 0
        specializations_s = ' ' +
            @specializations.
            to_hash.
            map do |name, value|
              "#{name.value}=#{value.inspect}"
            end.
            join(', ')
      end

      "{class #{as_module_name @name, 'class'}#{specializations_s}}"
    end
  end
end
