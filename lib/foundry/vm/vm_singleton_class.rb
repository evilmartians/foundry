module Foundry
  class VMSingletonClass < VMClass
    attr_reader :object

    define_mapped_ivars :object

    def vm_initialize(superclass, object)
      super(superclass, nil)

      @object = object
    end

    def inspect
      "{SingletonClass of #{@object.inspect}}"
    end
  end
end