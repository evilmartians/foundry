module Foundry
  class ::BasicObject
    def __vm_object?
      false
    end
  end

  class VMImmediate < ::BasicObject
    def __vm_object?
      true
    end

    def nil?
      false
    end

    def is_a?(klass)
      self.class.ancestors.include? klass
    end

    def respond_to?(method)
      self.class.method_defined? method
    end

    def method(method)
      self.class.instance_method(method)
    end

    def instance_variables
      []
    end

    def instance_variable_get(ivar)
      VI::NIL
    end

    def instance_variable_set(ivar, value)
      VI::NIL
    end
  end
end