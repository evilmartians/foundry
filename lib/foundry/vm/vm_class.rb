module Foundry
  class VMClass < VMModule
    attr_reader :superclass

    def initialize(klass, superclass, name=nil, vm_class=VMObject)
      super(klass, name)

      @vm_class   = vm_class
      @superclass = superclass
      @upperclass = superclass
    end

    def allocate(*args)
      unless @vm_class.ancestors.include? VMBasicObject
        ::Kernel.send :raise, ::Exception, "cannot allocate VMClass instance for VMImmediate"
      else
        @vm_class.new(self, *args)
      end
    end

    def _allocate(interp, scope)
      scope.self.allocate
    end

    def inspect
      if @superclass
        "{Class #{@name} < #{@superclass.name}}"
      else
        "{Class #{@name}"
      end
    end
  end
end