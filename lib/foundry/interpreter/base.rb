module Foundry::Interpreter
  class Base < Furnace::AST::Processor
    attr_reader :env, :outer

    def initialize(ast, self_=nil, args=nil, env=nil, outer=nil)
      @ast          = ast
      @self, @args  = self_, args
      @env          = env
      @outer        = outer

      @current_insn = nil
      @scope_stack  = []
    end

    #
    # Processing
    #

    def process(node)
      @current_insn = node
      super
    end

    def handler_missing(node)
      raise "Missing handler for node\n#{node.inspect}"
    end

    #
    # Stack traces
    #

    def collect_backtrace_part(include_current)
      last_function = nil

      if include_current
        stack = @scope_stack + [ @current_insn ]
      else
        stack = @scope_stack
      end

      stack.reverse.map do |insn|
        if insn.function
          last_function = insn.function
        end

        BacktraceItem.new(insn.file, insn.line, last_function).freeze
      end.freeze
    end

    def collect_backtrace(include_current=true)
      if @outer
        ([ collect_backtrace_part(include_current) ] + @outer.collect_backtrace(false)).freeze
      else
        [ collect_backtrace_part(include_current) ].freeze
      end
    end

    def evaluate
      process @ast
    end

    #
    # Initial contexts
    #

    def on_self(node)
      @self
    end

    def on_const_base(node)
      VI::Object
    end

    #
    # Variables
    #

    def process_let(node, flush_env)
      vars, *body = node.children

      if flush_env
        new_env = Environment.new

        if @env
          new_env.define(:Cref, @env.apply(:Cref))
        end
      else
        new_env = @env.extend
      end

      begin
        old_env = @env
        @env = new_env

        vars.each do |name, value|
          new_env.define name, process(value)
        end

        @scope_stack.push(node)

        process_all(body).last
      ensure
        @env = old_env
        @scope_stack.pop
      end
    end

    def on_let_new(node)
      process_let(node, true)
    end

    def on_let(node)
      process_let(node, false)
    end

    def on_var(node)
      var, = node.children

      @env.apply(var)
    end

    def on_mut!(node)
      var, value = node.children

      @env.mutate(var, process(value))
    end

    def on_eval_mut!(node)
      var, value = node.children

      if @env.defined?(var)
        @env.mutate(var, process(value))
      else
        @env.define(var, process(value))
      end
    end

    #
    # Control flow
    #

    def on_block(node)
      @scope_stack.push node

      process_all(node.children).last || VI::NIL
    ensure
      @scope_stack.pop
    end

    #
    # Tuples and de/composition
    #

    def on_array(node)
      result = []

      node.children.map do |child|
        if child.type == :splat
          result += process_all(child.children)
        else
          result << process(child)
        end
      end

      result
    end

    def on_array_unshift(node)
      array, value = process_all(node.children)

      [ value ] + array
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

    def parse_scoped_const(name_node)
      if name_node.type == :const_ref
        name,        = name_node.children
        outer_module = @env.apply(:Cref).first || VI::Object
      elsif name_node.type == :const_fetch
        outer_node, name = name_node.children
        outer_module = process(outer_node)
      end

      [ outer_module, name ]
    end
    protected :parse_scoped_const

    def find_const_in(scopes, name)
      scopes.each do |scope|
        if scope.const_defined?(name)
          return scope.const_get(name)
        end
      end

      return VI::UNDEF
    end

    def on_const_ref(node)
      name, = node.children

      const = find_const_in(@env.apply(:Cref), name)
      if const == VI::UNDEF
        const = find_const_in(@env.apply(:Cref).first.ancestors, name)
      end

      unless const == VI::UNDEF
        const
      else
        raise Error.new(self, "uninitialized constant #{name}")
      end
    end

    def on_const_fetch(node)
      parent_node, name = node.children

      modulus = process(parent_node)

      if !modulus.is_a? VI::Module
        raise Error.new(self, "#{modulus.inspect}:#{modulus.class.name} is not a class/module")
      else
        const = modulus.const_get(name)

        if const == VI::UNDEF
          raise Error.new(self, "uninitialized constant #{name} for #{modulus.name}")
        else
          const
        end
      end
    end

    def on_const_declare(node)
      name_node, value_node = node.children

      outer_module, name = parse_scoped_const(name_node)
      value = process(value_node)

      if outer_module.const_defined?(name)
        raise Error.new(self, "already initialized constant #{name}")
      else
        outer_module.const_set(name, value)
      end
    end

    #
    # Classes and modules
    #

    def on_define_module(node)
      name_node, = node.children

      outer_module, name = parse_scoped_const(name_node)

      modulus = outer_module.const_get(name)
      unless modulus == VI::UNDEF
        unless modulus.is_a? VI::Module
          raise Error.new(self, "#{name} is not a module")
        end
      else
        modulus = VI::Module.allocate(name)
        outer_module.const_set name, modulus
      end

      modulus
    end

    def on_define_class(node)
      name_node, superclass_node = node.children

      if superclass_node.nil?
        superclass = VI::Object
      else
        superclass = process(superclass_node)
      end

      outer_module, name = parse_scoped_const(name_node)

      klass = outer_module.const_get(name)
      unless klass == VI::UNDEF
        unless klass.is_a? VI::Class
          raise Error.new(self, "#{name} is not a class")
        end
      else
        klass = VI::Class.allocate(superclass, name)
        outer_module.const_set name, klass
      end

      klass
    end

    def on_defn(node)
      name, arguments_node, body_node = node.children

      @env.apply(:Defn).define_method(name, body_node)

      VI::NIL
    end

    def on_alias(node)
      (to_name, ), (from_name, ) = node.children.map(&:children)
      definee = @env.apply(:Defn)

      method = definee.instance_method(from_name)

      if method == VI::UNDEF && @env.apply(:Defn).is_a?(VI::Module)
        method = definee.method(from_name)
      end

      if method == VI::UNDEF
        raise Error.new(self, "undefined method #{from_name} for #{definee.name}")
      end

      definee.define_method(to_name, method)

      VI::NIL
    end

    #
    # Calls
    #

    def on_call(node)
      receiver_node, name, arguments_node = node.children

      if arguments_node
        arguments = process(arguments_node)
      else
        arguments = []
      end

      if receiver_node
        receiver = process(receiver_node)
      else
        receiver = @env.apply(:Self)
      end

      if receiver.respond_to? name
        method = receiver.method(name)

        Foundry::Runtime.interpreter.
          new(method, self, arguments, nil, self).
          evaluate
      else
        raise Error.new(self, "undefined method #{name} for #{receiver.class.name}")
      end
    end
  end
end