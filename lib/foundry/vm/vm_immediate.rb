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

    def _equal?(interp, scope)
      other = scope.arguments.first
      equal?(other) ? VI::TRUE : VI::FALSE
    end

    def _send(interp, scope)
      method_name = scope.arguments.first.value # TODO
      method = scope.self.method(method_name)

      scope = VariableScope.new(scope.self, scope.module, nil,
            scope.const_scope, scope.arguments[1..-1], scope.block)
      scope.function = method_name.to_s
      method.execute(interp, scope)
    end
  end
end