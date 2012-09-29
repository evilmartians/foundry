module Foundry
  class InterpreterError < StandardError
  end

  class Interpreter
    AST = Melbourne::AST

    def initialize(executable, scope)
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

    def evaluate
      visit @executable.ast
    end

    def visit(node)
      case node
      when AST::NilLiteral
        VI::NIL
      when AST::TrueLiteral
        VI::TRUE
      when AST::FalseLiteral
        VI::FALSE

      when AST::SymbolLiteral
        VMSymbol.new(node.value)
      when AST::FixnumLiteral
        VI::Integer.allocate(node.value)
      when AST::StringLiteral
        VI::String.allocate(node.string)

      when AST::Self
        @scope.self

      when AST::ConstantAccess
        process_constant_access(node.name)
      when AST::ScopedConstant
        process_scoped_constant(node.name, node.parent)
      when AST::ConstantAssignment
        process_constant_assignment(node.constant, node.value)

      when AST::Block
        process_block(node.array)

      when AST::Begin
        process_begin(node.rescue)

      when AST::Module
        process_module(node.name, node.body)
      when AST::Class
        process_class(node.name, node.superclass, node.body)
      when AST::Define
        process_define(node.name, node.arguments, node.body)
      when AST::Alias
        process_alias(node.from, node.to)

      when AST::LocalVariableAccess
        @scope.locals[node.name]
      when AST::LocalVariableAssignment
        @scope.locals[node.name] = visit(node.value)

      when AST::SendWithArguments
        process_send(node.receiver, node.name, node.arguments, node.block,
                node.check_for_local, node.privately)
      when AST::Send
        process_send(node.receiver, node.name, nil, node.block,
                node.check_for_local, node.privately)

      else
        raise ::Exception, "unknown node #{node.class} on #{node.pretty_inspect}"
      end
    end

    def process_constant_access(name)
      if const = @scope.const_scope.find_const(name)
        const
      else
        raise InterpreterError, "uninitialized constant #{name}"
      end
    end

    def process_scoped_constant(name, parent_node)
      modulus = visit(parent_node)

      if const = modulus.const_get(name)
        const
      else
        raise InterpreterError, "uninitialized constant #{name}"
      end
    end

    def process_constant_assignment(constant_node, value_node)
      if constant_node.is_a? AST::ScopedConstant
        modulus = visit(constant_node.parent)
      else
        modulus = @scope.const_scope.nesting.first
      end

      value = visit(value_node)

      if modulus.const_defined?(constant_node.name)
        raise InterpreterError, "already initialized constant #{constant_node.name}"
      else
        modulus.const_set(constant_node.name, value_node)
      end
    end

    def process_block(code)
      result = VI::NIL
      code.compact.each do |statement|
        result = visit statement
      end
      result
    end

    def push_cref_and_process_body(modulus, body_node)
      unless body_node.is_a? AST::EmptyBody
        const_scope = @scope.const_scope.nest(modulus)
        scope = VariableScope.new(modulus, modulus, nil, const_scope, [], nil)

        with_scope(scope) do
          visit body_node.body
        end
      end
    end
    protected :push_cref_and_process_body

    def process_module(name_node, body_node)
      name = name_node.name
      if name_node.is_a? AST::ModuleName
        outer_module = @scope.const_scope.nesting.first
      elsif name_node.is_a? AST::ScopedModuleName
        outer_module = visit(name_node.parent)
      end

      modulus = outer_module.const_get(name)
      unless modulus == VI::UNDEF
        unless modulus.is_a? VI::Module
          raise InterpreterError, "#{name} is not a module"
        end
      else
        modulus = VI::Module.allocate(name)
        outer_module.const_set name, modulus
      end

      push_cref_and_process_body(modulus, body_node)
    end

    def process_class(name_node, superclass_node, body_node)
      superclass = visit(superclass_node)
      superclass = VI::Object if superclass.nil?

      name = name_node.name
      if name_node.is_a? AST::ClassName
        outer_module = @scope.const_scope.nesting.first
      elsif name_node.is_a? AST::ScopedClassName
        outer_module = visit(name_node.parent)
      end

      klass = outer_module.const_get(name)
      unless klass == VI::UNDEF
        unless klass.is_a? VI::Class
          raise InterpreterError, "#{name} is not a class"
        end
      else
        klass = VI::Class.allocate(superclass, name)
        outer_module.const_set name, klass
      end

      push_cref_and_process_body(klass, body_node)
    end

    def extract_primitive_node(body_node)
      first_statement = body_node.array.first
      if first_statement.is_a?(AST::SendWithArguments) &&
            first_statement.receiver.is_a?(AST::ConstantAccess) &&
            first_statement.receiver.name == :Foundry &&
            first_statement.name == :primitive &&
            first_statement.arguments.array.length == 1

        first_argument = first_statement.arguments.array.first

        if first_argument.is_a?(AST::SymbolLiteral)
          body_node.array.delete first_statement

          first_argument.value
        end
      end
    end
    protected :extract_primitive_node

    def process_define(name, arguments_node, body_node)
      primitive = extract_primitive_node(body_node)
      method = MethodBody.new(body_node, @scope.module, arguments_node, primitive)

      @scope.module.define_method(name, method)

      VI::NIL
    end

    def process_alias(from_node, to_node)
      method = @scope.module.instance_method(from_node.value)

      if method == VI::UNDEF
        raise InterpreterError, "undefined method #{name} for #{receiver.class.name}"
      end

      @scope.module.define_method(to_node.value, method)

      VI::NIL
    end

    def create_proc_from_block(block_node)
      body = ClosureBody.new(block_node)
      BlockEnvironment.new(@scope, body, true)
    end
    protected :create_proc_from_block

    def expand_arguments(arguments_node)
      arguments_node.array.map do |elem|
        visit elem
      end
    end
    protected :expand_arguments

    def process_send(receiver_node, name, arguments_node,
              block_node, check_for_local, privately)
      # MELBOURNE should emit check_for_local, but it doesn't
      if @scope.eval_scope &&
            arguments_node.nil? &&
            @scope.locals.has_key?(name)
        return @scope.locals[name]
      end

      if block_node
        block = create_proc_from_block(block_node)
      end

      if arguments_node
        arguments = expand_arguments(arguments_node)
      else
        arguments = []
      end

      receiver = visit(receiver_node)

      if receiver.respond_to? name
        method = receiver.method(name)

        const_scope = ConstantScope.new([ method.module ])
        scope = VariableScope.new(receiver, method.module, nil,
                  const_scope, arguments, block)

        method.execute(scope)
      else
        raise InterpreterError, "undefined method #{name} for #{receiver.class.name}"
      end
    end

    def process_begin(rescue_node)
      # Ignore for now
      raise NotImplementedError if rescue_node

      VI::NIL
    end
  end
end