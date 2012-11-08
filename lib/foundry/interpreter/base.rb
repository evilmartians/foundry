module Foundry
  class Interpreter::Base
    include Furnace::AST::Processor

    attr_reader :outer

    def initialize(outer, executable, scope)
      @outer       = outer
      @executable  = executable
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
    protected :with_scope

    def innermost_scope
      @scope
    end

    def collect_backtrace_part
      ([ @scope ] + @scope_stack).map do |scope|
        Interpreter::BacktraceItem.new(@executable.file, scope.line, scope.function).freeze
      end.freeze
    end

    def collect_backtrace
      if @outer
        ([ collect_backtrace_part ] + @outer.collect_backtrace).freeze
      else
        [ collect_backtrace_part ].freeze
      end
    end

    def evaluate
      process @executable.ast
    end

    #
    # Processor
    #

    def process(node)
      @scope.line = node.line
      super
    end

    def handler_missing(node)
      raise "Missing handler for node #{node.inspect}"
    end

    #
    # Literals
    #

    def on_nil(node)
      VI::NIL
    end

    def on_false(node)
      VI::FALSE
    end

    def on_true(node)
      VI::TRUE
    end

    def on_symbol(node)
      value, = node.children
      VMSymbol.new(value)
    end

    def on_integer(node)
      value, = node.children
      VI::Integer.allocate(value)
    end

    def on_string(node)
      value, = node.children
      VI::String.allocate(value)
    end

    #
    # Constants
    #

    def push_cref_and_process_body(modulus, body_node)
      if body_node
        const_scope = @scope.const_scope.nest(modulus)
        scope = VariableScope.new(modulus, modulus, nil, const_scope, [], nil)

        if modulus.is_a? VI::Class
          scope.function = "<class #{modulus.name}>" || '(anonymous class)'
        else
          scope.function = "<module #{modulus.name}>" || '(anonymous module)'
        end

        with_scope(scope) do
          process body_node
        end
      else
        VI::NIL
      end
    end
    protected :push_cref_and_process_body

    def parse_scoped_const(name_node)
      if name_node.is_a? Symbol
        outer_module = @scope.const_scope.nesting.first
        name         = name_node
      elsif name_node.type == :colon2
        outer_node, name = name_node.children
        outer_module = process(outer_node)
      end

      [ outer_module, name ]
    end
    protected :parse_scoped_const

    def on_const(node)
      name, = node.children

      const = @scope.const_scope.find_const(name)
      unless const == VI::UNDEF
        const
      else
        raise Interpreter::Error.new(self, "uninitialized constant #{name}")
      end
    end

    def on_colon2(node)
      parent_node, name = node.children

      modulus = process(parent_node)

      const = modulus.const_get(name)
      unless const == VI::UNDEF
        const
      else
        raise Interpreter::Error.new(self, "uninitialized constant #{name} for #{modulus.name}")
      end
    end

    def on_cdecl(node)
      name_node, value_node = node.children

      outer_module, name = parse_scoped_const(name_node)
      value = process(value_node)

      if outer_module.const_defined?(name)
        raise Interpreter::Error.new(self, "already initialized constant #{name}")
      else
        outer_module.const_set(name, value)
      end
    end

    #
    # Classes and modules
    #

    def on_module(node)
      name_node, body_node = node.children

      outer_module, name = parse_scoped_const(name_node)

      modulus = outer_module.const_get(name)
      unless modulus == VI::UNDEF
        unless modulus.is_a? VI::Module
          raise Interpreter::Error.new(self, "#{name} is not a module")
        end
      else
        modulus = VI::Module.allocate(name)
        outer_module.const_set name, modulus
      end

      push_cref_and_process_body(modulus, body_node)
    end

    def on_class(node)
      name_node, superclass_node, body_node = node.children

      if superclass_node.nil?
        superclass = VI::Object
      else
        superclass = process(superclass_node)
      end

      outer_module, name = parse_scoped_const(name_node)

      klass = outer_module.const_get(name)
      unless klass == VI::UNDEF
        unless klass.is_a? VI::Class
          raise Interpreter::Error.new(self, "#{name} is not a class")
        end
      else
        klass = VI::Class.allocate(superclass, name)
        outer_module.const_set name, klass
      end

      push_cref_and_process_body(klass, body_node)
    end

    def on_defn(node)
      name, arguments_node, body_node = node.children

      method = MethodBody.new(body_node, @executable.file, @scope.module, arguments_node)

      @scope.module.define_method(name, method)

      VI::NIL
    end

    def on_alias(node)
      (to_name, ), (from_name, ) = node.children.map(&:children)

      method = @scope.module.instance_method(from_name)

      if method == VI::UNDEF && @scope.module.is_a?(VI::Module)
        method = @scope.module.method(from_name)
      end

      if method == VI::UNDEF
        raise Interpreter::Error.new(self, "undefined method #{from_name} for #{@scope.module.name}")
      end

      @scope.module.define_method(to_name, method)

      VI::NIL
    end

    #
    # Variables
    #

    def on_lvar(node)
      var, = node.children
      @scope.locals[var]
    end

    def on_lasgn(node)
      var, value = node.children
      @scope.locals[var] = process(value)
    end

    #
    # Tuples and de/composition
    #

    def on_array(node)
      process_all(node.children)
    end

    #
    # Calls
    #

    def on_block(node)
      process_all(node.children).last || VI::NIL
    end

    def on_call(node)
      receiver_node, name, arguments_node = node.children

      # Melbourne-specific snippet emulating NODE_VCALL
      if receiver_node.nil? && arguments_node.nil? &&
            @scope.locals.has_key?(name)
        return @scope.locals[name]
      end

      if arguments_node
        arguments = process(arguments_node)
      else
        arguments = []
      end

      if receiver_node
        receiver = process(receiver_node)
      else
        receiver = @scope.self
      end

      if receiver.respond_to? name
        method = receiver.method(name)

        const_scope = ConstantScope.new([ method.module ])
        scope = VariableScope.new(receiver, method.module, nil,
                  const_scope, arguments, nil)
        scope.function = name.to_s

        method.execute(self, scope)
      else
        raise Interpreter::Error.new(self, "undefined method #{name} for #{receiver.class.name}")
      end
    end

  end
end