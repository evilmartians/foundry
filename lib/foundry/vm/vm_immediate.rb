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
      equal? VI::NIL
    end

    def to_s
      inspect
    end

    def singleton_class
      self.class
    end

    def singleton_class_defined?
      false
    end

    def is_a?(klass)
      if singleton_class_defined?
        self.singleton_class.ancestors.include? klass
      else
        self.class.ancestors.include? klass
      end
    end

    def respond_to?(method)
      if singleton_class_defined?
        self.singleton_class.method_defined? method
      else
        self.class.method_defined? method
      end
    end

    def method(method)
      if singleton_class_defined?
        self.singleton_class.instance_method method
      else
        self.class.instance_method method
      end
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