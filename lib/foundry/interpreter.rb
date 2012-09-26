require 'pp'

module Foundry
  class InterpreterError < StandardError
  end

  class Interpreter
    AST = Melbourne::AST

    def initialize(scope)
      @scope       = scope
      @scope_stack = []
    end

    def with_scope(scope)
      @scope_stack.push @scope
      @scope = scope

      yield
    ensure
      @scope = @scope_stack.pop
    end
    private :with_scope

    def evaluate
      visit @scope.method.ast
    end

    def visit(node)
      case node
      when AST::Nil
        VI::NIL
      when AST::True
        VI::TRUE
      when AST::False
        VI::FALSE

      when AST::ConstFind
        process_const_find(node.name)
      when AST::ConstAccess
        process_const_access(node.name, node.parent)

      when AST::Block
        process_block(node.array)

      when AST::Class
        process_class(node.name, node.superclass, node.body)
      when AST::Define
        process_define(node.name, node.arguments, node.body)

      else
        raise ::Exception, "unknown node #{node.class} on #{node.pretty_inspect}"
      end
    end

    def process_const_find(name)
      if const = @scope.const_scope.find_const(name)
        const
      else
        raise InterpreterError, "uninitialized constant #{name}"
      end
    end

    def process_const_access(name, parent_node)
      module_ = visit(parent_node)

      if const = module_.const_get(name)
        const
      else
        raise InterpreterError, "uninitialized constant #{name}"
      end
    end

    def process_block(code)
      result = VI::NIL
      code.each do |statement|
        result = visit statement
      end
      result
    end

    def process_class(name_node, superclass_node, body_node)
      superclass = visit(superclass_node)
      superclass = VI::Object if superclass.vm_nil?

      name = name_node.name
      if name_node.is_a? AST::ClassName
        outer_class = VI::Object
      elsif name_node.is_a? AST::ScopedClassName
        outer_class = visit(name_node.parent)
      end

      if klass = outer_class.const_get(name)
        unless klass.is_a? VI::Class
          raise InterpreterError, "#{name} is not a class"
        end
      else
        klass = VI::Class.allocate(superclass, name)
        outer_class.const_set name, klass
      end

      const_scope = @scope.const_scope.nest(klass)
      scope = VariableScope.new(@scope.method, klass, nil, const_scope, [], nil)

      with_scope(scope) do
        visit body_node.body
      end
    end

    def process_define(name, arguments_node, body_node)
      method = Method.new(body_node, arguments_node)
      @scope.module.define_method(name, method)
    end
  end
end