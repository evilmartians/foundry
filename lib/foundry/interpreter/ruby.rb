module Foundry
  class Interpreter::Ruby < Interpreter::Base
    #
    # Objects
    #

    def on_equal?(node)
      self_ = @scope.self
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
      @scope.self.allocate
    end

    def on_include(node)
      @scope.self.include(@scope.arguments.first) # TODO
      #@scope.self.include(visit(node.children.first))
    end

  end
end