module Foundry
  module Arch::Common
    include Primitives

    def self.eval_include(interp, scope, args)
      scope.self.include(*scope.arguments) # TODO
    end

    def self.eval_equal?(interp, scope, args)
      self_ = scope.self
      other = scope.arguments.first # TODO

      if self_.is_a? VI::Symbol
        (self_.value == other.value) ? VI::TRUE : VI::FALSE
      else
        self_.equal? other
      end
    end

    def self.eval_send(interp, scope, args)
      method_name = scope.arguments.first.value # TODO
      method = scope.self.method(method_name)

      scope = VariableScope.new(scope.self, scope.module, nil,
            scope.const_scope, scope.arguments[1..-1], scope.block)
      scope.function = method_name.to_s
      method.execute(interp, scope)
    end

    def self.eval_allocate(interp, scope, args)
      scope.self.allocate
    end
  end
end