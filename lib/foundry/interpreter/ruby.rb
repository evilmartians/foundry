module Foundry::Interpreter
  class Ruby < Base
    #
    # Objects
    #

    def on_equal?(node)
      self_ = apply_env(:Self)
      other = visit(node.arguments.first)

      if self_.is_a? VI::Symbol
        (self_.value == other.value) ? VI::TRUE : VI::FALSE
      else
        self_.equal? other
      end
    end

    def on_send(node)
      method_name = visit(node.children.first).value # TODO to_sym
      method      = @scope.self.method(method_name)

      scope = VariableScope.new(@scope.self, @scope.module, nil,
            @scope.const_scope, visit_all(node.children.drop(1)), @scope.block)
      scope.function = method_name.to_s
      method.execute(interp, scope)
    end

    #
    # Classes and modules
    #

    def on_allocate(node)
      apply_env(:Self).allocate
    end

    def on_include(node)
      modulus, = process_all(node.children)
      apply_env(:Self).include(modulus)
    end

  end
end